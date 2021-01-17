{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
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
import Control.Monad.Extra

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

newtype Markov = Markov
  { mecab :: MeCab.MeCab }

instance Bot Markov where
  type DepF Markov = HasLogFunc :&: HasMarkovConfig

  name _ = "Markov"

  init = do
    m <- liftIO $ MeCab.new []
    retry 5 10 $ runSql (runMigration migrateAll)
    return $ Markov m

  reply (Markov mecab) (Message _ msg) = runSql $ do
    unless (isTriggerMsg msg) $ learn mecab msg
    whenMaybeM (isTimeToSay msg) say

type DbRIO e a = ReaderT SqlBackend (LoggingT (ResourceT (RIO e))) a

isTimeToSay :: Text -> DbRIO e Bool
isTimeToSay inputMsg = do
  r <- rand (1, 1000)
  return $ r <= 1 || isTriggerMsg inputMsg

isTriggerMsg :: Text -> Bool
isTriggerMsg = (==) "のこ"

rand :: (Int, Int) -> DbRIO e Int
rand (min, max) = liftIO $ getStdRandom $ randomR @Int (min, max)

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
  ns <- liftIO $ MeCab.parseToNodes mecab msg
  let ms = deleteBosEos $ map mkMorpheme ns
  mIds <- insertMorphemes ms
  insertTransitions $ to3gram mIds

say :: DbRIO e Text
say = mkSentence (Nothing, Nothing, Nothing) ""

mkSentence :: (Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId) -> Text -> DbRIO e Text
mkSentence mps@(mp2, mp1, mp0) txt' = do
  ts <- fetchTransitionsByMorphemes mps
  t <- pickRandTransition ts
  txt <- appendTransitionWord t txt'
  case transitionNextMorp t of
    Nothing -> return txt
    Just m -> mkSentence (mp1, mp0, Just m) txt

fetchTransitionsByMorphemes :: (Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId) -> DbRIO e [Transition]
fetchTransitionsByMorphemes (mp2, mp1, mp0) = do
  es <- selectList 
    [ TransitionPrev2Morp ==. mp2
    , TransitionPrev1Morp ==. mp1
    , TransitionPrev0Morp ==. mp0
    ] []
  return $ map entityVal es

appendTransitionWord :: Transition -> Text -> DbRIO e Text
appendTransitionWord t txt = do
  case transitionPrev0Morp t of
    Nothing  -> return txt
    Just mId -> do
      m <- getJust mId
      return $ txt `T.append` morphemeWord m

pickRandTransition :: [Transition] -> DbRIO e Transition
pickRandTransition ts = do
  let totalCnt = sum $ map transitionCount ts
  r <- rand (1, totalCnt)
  case pick transitionCount r ts of
    Nothing -> throwString "Bug: pick function underflow"
    Just t -> return t

pick :: (a -> Int) -> Int -> [a] -> Maybe a
pick _ _ [] = Nothing
pick f n (t : ts)
  | n <= 0 = Nothing
  | n <= f t = Just t
  | otherwise = pick f (n - f t) ts

insertTransitions :: [(Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId)] -> DbRIO e ()
insertTransitions = go Nothing . reverse where
  go :: Maybe MorphemeId -> [(Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId)] -> DbRIO e ()
  go _ [] = return ()
  go mn (mt@(_, _, m0) : mts) = do
    upsertTransition mt mn 
    go m0 mts

upsertTransition :: (Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId) -> Maybe MorphemeId -> DbRIO e ()
upsertTransition mMs mMn = maybeM (insertTransition mMs mMn) incrementTransitionCount (fetchTransitionId mMs mMn)

incrementTransitionCount :: TransitionId -> DbRIO e ()
incrementTransitionCount tId = update tId [TransitionCount +=. 1]

insertTransition :: (Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId) -> Maybe MorphemeId -> DbRIO e ()
insertTransition (mM2, mM1, mM0) mMn = do
  now <- getCurrentTime
  void $ insert (Transition mM2 mM1 mM0 mMn now 1)

fetchTransitionId :: (Maybe MorphemeId, Maybe MorphemeId, Maybe MorphemeId) -> Maybe MorphemeId -> DbRIO e (Maybe TransitionId)
fetchTransitionId (mM2, mM1, mM0) mMn = do
  mE <- selectFirst 
    [ TransitionPrev2Morp ==. mM2
    , TransitionPrev1Morp ==. mM1
    , TransitionPrev0Morp ==. mM0
    , TransitionNextMorp  ==. mMn
    ] []
  return $ entityKey <$> mE

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

