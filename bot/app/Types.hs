module Types where

import NokoBot
import RIO
import RIO.Process
import Database.Persist.MySQL (ConnectInfo (..))
import Servant (ServerT)

data MyEnv a = MyEnv
  { logFunc :: LogFunc
  , processContext :: ProcessContext
  , envData :: a
  }

envDataL :: Lens' (MyEnv a) a
envDataL = lens envData (\d v -> d{envData = v})

instance HasLogFunc (MyEnv a) where
  logFuncL = lens logFunc (\d v -> d{logFunc = v})

instance HasProcessContext (MyEnv a) where
  processContextL = lens processContext (\d v -> d{processContext = v})

data Config = Config
  { dbInfo :: ConnectInfo
  }

instance HasMarkovConfig Config where
  dbConfigL = lens dbInfo (\d v -> d{dbInfo = v})

instance HasMarkovConfig (MyEnv Config) where
  dbConfigL = envDataL . dbConfigL

instance HasMarkovConfig (Config, InitVal) where
  dbConfigL = lens fst (\(_,e) v -> (v,e)) . dbConfigL

instance HasMarkovConfig (MyEnv (Config, InitVal)) where
  dbConfigL = envDataL . dbConfigL

data InitVal = InitVal
  { allBots :: [SomeBot AllBotConstraints]
  }

class HasAllBots a where
  allBotsL :: Lens' a [SomeBot AllBotConstraints]

instance HasAllBots InitVal where
  allBotsL = lens allBots (\d v -> d{allBots = v})

instance HasAllBots (Config, InitVal) where
  allBotsL = lens snd (\(e,_) v -> (e,v)) . allBotsL

instance HasAllBots (MyEnv (Config, InitVal)) where
  allBotsL = envDataL . allBotsL

type MyServer api = ServerT api (RIO (MyEnv (Config, InitVal)))

