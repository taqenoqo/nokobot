{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module NokoBot.Bots.Markov 
  ( Markov
  , HasMarkovConfig(..)
  ) where

import RIO
import RIO.Time
import Database.Persist
import Database.Persist.TH
import Database.Persist.MySQL
import NokoBot.Types
import qualified RIO.Text as T
import qualified Text.MeCab as MeCab
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Conduit (ResourceT, runResourceT)
import System.Random (getStdRandom, randomR)

share 
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrateAll" 
  ] [persistLowerCase|
    Morpheme
      word Text
      wordClass Text
      UniqMorpheme word wordClass
      deriving Show

    Transition
      prev2Morp MorphemeId Maybe
      prev1Morp MorphemeId Maybe
      prev0Morp MorphemeId Maybe
      nextMorp MorphemeId Maybe
      created UTCTime
      count Int
      deriving Show
  |]

class HasMarkovConfig env where
  dbConfigL :: Lens' env ConnectInfo

data Markov = Markov
  { mecab :: MeCab.MeCab }

instance Bot (Markov) where

  type DepF Markov = HasLogFunc :&: HasMarkovConfig

  name _ = "Markov"

  init = do
    m <- liftIO $ MeCab.new []
    retry 5 10 $ runSql (runMigration migrateAll)
    return $ Markov m

  reply (Markov mecab) (Message _ msg) = runSql $ do
    learn mecab msg
    response <- say
    return $ Just response

type DbRIO e a = ReaderT SqlBackend (LoggingT (ResourceT (RIO e))) a

retry :: HasLogFunc e => Int -> Int -> RIO e a -> RIO e a
retry n delay act
  | n <= 1 = act
  | otherwise = act `catch` \(e :: SomeException) -> do
    logInfo $ displayShow e
    threadDelay $ delay * 1000 * 1000
    retry (n - 1) (delay + 10) act

runSql :: HasMarkovConfig e => DbRIO e a -> RIO e a
runSql rio = do
  connectInfo <- view dbConfigL
  runResourceT $ runStdoutLoggingT $ withMySQLConn connectInfo $ runSqlConn rio

learn :: MeCab.MeCab -> Text -> DbRIO e ()
learn mecab msg = do
  utc <- getCurrentTime
  ns <- liftIO $ MeCab.parseToNodes mecab msg
  let ms = deleteBosEos $ map mkMorpheme ns
  mIds <- insertMorphemes ms
  insertTransitions utc (to3gram mIds)

say :: DbRIO e (Text)
say = do
  mkSentence (Nothing, Nothing, Nothing) ""

mkSentence :: (Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId) -> Text -> DbRIO e Text
mkSentence (mp2, mp1, mp0) s' = do
  es <- selectList 
    [ TransitionPrev2Morp ==. mp2
    , TransitionPrev1Morp ==. mp1
    , TransitionPrev0Morp ==. mp0
    ] []
  let ts = map entityVal es
  t <- pickRandTransition ts
  s <- appendTransitionWord t s'
  case transitionNextMorp t of
    Nothing -> return s
    Just m -> mkSentence (mp1, mp0, Just m) s

appendTransitionWord :: Transition -> Text -> DbRIO e Text
appendTransitionWord t s = do
  case transitionPrev0Morp t of
    Nothing  -> return s
    Just mId -> do
      m <- getJust $ mId
      return $ s `T.append` morphemeWord m

pickRandTransition :: [Transition] -> DbRIO e Transition
pickRandTransition ts = do
  let totalCnt = sum $ map transitionCount ts
  r <- liftIO $ getStdRandom $ randomR (1, totalCnt)
  case pick transitionCount r ts of
    Nothing -> error "consistency error."
    Just t -> return t

pick :: (a -> Int) -> Int -> [a] -> Maybe a
pick _ _ [] = Nothing
pick f n (t : ts)
  | n <= 0 = Nothing
  | n <= f t = Just t
  | otherwise = pick f (n - f t) ts

insertTransitions :: UTCTime -> [(Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId)] -> DbRIO e ()
insertTransitions utc = ins Nothing . reverse where
  ins :: Maybe MorphemeId -> [(Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId)] -> DbRIO e ()
  ins _ [] = return ()
  ins mId ((mp2, mp1, mp0) : ms) = do
    em <- selectFirst 
      [ TransitionPrev2Morp ==. mp2
      , TransitionPrev1Morp ==. mp1
      , TransitionPrev0Morp ==. mp0
      , TransitionNextMorp  ==. mId
      ] []
    case em of
      Nothing -> do
        insert (Transition mp2 mp1 mp0 mId utc 1)
      Just e -> do
        update (entityKey e) [TransitionCount +=. 1]
        return $ entityKey e
    ins mp0 ms

deleteBosEos :: [Morpheme] -> [Morpheme]
deleteBosEos ms = [ m | m <- ms, morphemeWord m /= "" ]

insertMorphemes :: [Morpheme] -> DbRIO e [MorphemeId]
insertMorphemes ms = map entityKey <$> for ms (`upsert` [])

mkMorpheme :: MeCab.Node Text -> Morpheme
mkMorpheme n = Morpheme (MeCab.nodeSurface n) $ MeCab.nodeFeature n

to3gram :: [a] -> [(Maybe a, Maybe a, Maybe a)]
to3gram = reverse . to3gram' . reverse where
  to3gram' :: [a] -> [(Maybe a, Maybe a, Maybe a)]
  to3gram' []            = [(Nothing, Nothing, Nothing)]
  to3gram' [p0]          = (Nothing, Nothing, Just p0) : to3gram' []
  to3gram' [p0, p1]      = (Nothing, Just p1, Just p0) : to3gram' [p1]
  to3gram' (p0:p1:p2:xs) = (Just p2, Just p1, Just p0) : to3gram' (p1:p2:xs)

