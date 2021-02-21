{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Env
import NokoBot
import Servant
import RIO hiding (Handler)
import RIO.Process
import qualified RIO.Map as Map
import qualified RIO.Text as T
import RIO.Time (getZonedTime)
import Database.Persist.MySQL (ConnectInfo (..), defaultConnectInfo)
import Network.Wai.Handler.Warp (run)
import Control.Lens.TH (makeClassy_)

makeClassy_ ''ConnectInfo

(.~?) :: ASetter' s a -> Maybe a -> s -> s
(.~?) l = maybe id (l .~)
infixr 4 .~?

type RioServer api env = ServerT api (RIO env)

hoistRioServer :: Proxy Api -> RioServer Api env -> env -> Server Api
hoistRioServer p s e = hoistServer p (runRIO e) s

mkApplication :: RunTimeEnv -> Application
mkApplication env = serve api $ hoistRioServer api server env

main :: IO ()
main = runSimpleApp $ do
  config <- mkConfig
  env <- runInit config initialize
  let app = mkApplication env
  liftIO $ run 3000 app

mkConfig :: RIO SimpleApp Config
mkConfig = Config <$> mkDbInfo where
  mkDbInfo :: RIO SimpleApp ConnectInfo
  mkDbInfo = do
    envs <- view envVarsL
    return $ defaultConnectInfo
      & _connectHost .~? (T.unpack <$> envs Map.!? "MYSQL_HOST")

initialize :: RIO InitTimeEnv RunTimeEnv
initialize = do
  logInfo "Starting server..."
  RunTimeEnv
    <$> view rioEnv
    <*> view config
    <*> initAllBots

api :: Proxy Api
api = Proxy

type Api =
  "bot" :> ReqBody '[JSON] Text :> Post '[JSON] Text

server :: RioServer Api RunTimeEnv
server = postBot

postBot :: Text -> RIO RunTimeEnv Text
postBot input = do
  now <- getZonedTime
  bs <- view runTimeEnvAllBots
  r <- reply bs $ Message now input
  case r of
    Just s -> return s
    Nothing -> return ""

