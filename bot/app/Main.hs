{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Types
import NokoBot
import Servant
import RIO hiding (Handler)
import RIO.Process
import qualified RIO.Map as Map
import qualified RIO.Text as T
import RIO.Time (getZonedTime)
import Database.Persist.MySQL (ConnectInfo (..), defaultConnectInfo)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  config <- runSimpleApp mkConfig
  initVal <- runMyRIO config initialize
  run 3000 $ mkApplication config initVal

mkConfig :: RIO SimpleApp Config
mkConfig = Config <$> mkDbInfo where
  mkDbInfo :: RIO SimpleApp ConnectInfo
  mkDbInfo = do
    envs <- view envVarsL
    return $ defaultConnectInfo{connectSSL = Nothing}
      & setByMaybe (\v i -> i {connectHost = T.unpack v}) (envs Map.!? "MYSQL_HOST")

setByMaybe :: (a -> s -> s) -> Maybe a -> s -> s
setByMaybe = maybe id

initialize :: RIO (MyEnv Config) InitVal
initialize = do
  logInfo "Starting server."
  InitVal <$> initAllBots

mkApplication :: Config -> InitVal -> Application
mkApplication config iv = serve api $ hoistMyServer api server config iv

hoistMyServer :: Proxy Api -> MyServer Api -> Config -> InitVal -> Server Api
hoistMyServer a s config iv = hoistServer a (runMyRIO (config, iv)) s where

runMyRIO :: MonadIO m => e -> RIO (MyEnv e) a -> m a
runMyRIO envData rio = runSimpleApp $ do
  lo <- logOptionsHandle stdout False
  withLogFunc lo $ \lf -> do
    pc <- mkDefaultProcessContext
    runRIO (MyEnv lf pc envData) rio

api :: Proxy Api
api = Proxy

type Api =
  "bot" :> ReqBody '[JSON] Text :> Post '[JSON] Text

server :: MyServer Api
server = postBot

postBot :: (HasAllBots e, AllBotConstraints e) => Text -> RIO e Text
postBot input = do
  now <- getZonedTime
  bs <- view allBotsL
  r <- reply bs $ Message now input
  case r of
    Just s -> return s
    Nothing -> return "nothing"

