{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Env where

import NokoBot
import RIO
import RIO.Process
import Database.Persist.MySQL (ConnectInfo (..))
import Servant (ServerT)
import Control.Lens (makeClassy)

data Config = Config
  { _configDbInfo :: ConnectInfo }
makeClassy ''Config

instance HasConfig a => HasMarkovConfig a where
  dbConfigL = configDbInfo

data RioEnv = RioEnv
  { _rioEnvLogFunc :: LogFunc
  , _rioEnvProcessContext :: ProcessContext }
makeClassy ''RioEnv

instance {-# OVERLAPPABLE #-} HasRioEnv a => HasLogFunc a where
  logFuncL = rioEnvLogFunc

instance {-# OVERLAPPABLE #-} HasRioEnv a => HasProcessContext a where
  processContextL = rioEnvProcessContext

data InitTimeEnv = InitTimeEnv
  { _initTimeEnvRioEnv :: RioEnv
  , _initTimeEnvConfig :: Config }
makeClassy ''InitTimeEnv

instance HasRioEnv InitTimeEnv where
  rioEnv = initTimeEnvRioEnv

instance HasConfig InitTimeEnv where
  config = initTimeEnvConfig

runInit :: MonadUnliftIO m => Config -> RIO InitTimeEnv a -> m a
runInit config rio = withRioEnv $ \rioEnv ->
  let env = InitTimeEnv rioEnv config
  in runRIO env rio

withRioEnv :: MonadUnliftIO m => (RioEnv -> m a) -> m a
withRioEnv f = do
  lo <- logOptionsHandle stderr False
  withLogFunc lo $ \lf -> do
    pc <- mkDefaultProcessContext
    f $ RioEnv lf pc

data RunTimeEnv = RunTimeEnv
  { _runTimeEnvRioEnv :: RioEnv
  , _runTimeEnvConfig :: Config
  , _runTimeEnvAllBots :: [SomeBot AllBotConstraints] }
makeClassy ''RunTimeEnv

instance HasRioEnv RunTimeEnv where
  rioEnv = runTimeEnvRioEnv

instance HasConfig RunTimeEnv where
  config = runTimeEnvConfig

