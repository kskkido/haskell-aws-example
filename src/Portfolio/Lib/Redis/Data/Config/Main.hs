module Portfolio.Lib.Redis.Data.Config.Main
  ( Config(..)
  , HasConfig(..)
  , toConnectInfo
  ) where

import RIO
import qualified Data.Time as Time
import qualified Database.Redis as Redis

data Config = Config
  { host :: String
  , port :: Int
  , database :: Int
  , maxConnections :: Int
  , maxIdleTime :: Time.NominalDiffTime
  }
  deriving (Eq, Show)

class HasConfig a where
  get :: a -> Config
instance HasConfig Config where
  get = id

toConnectInfo :: Config -> Redis.ConnectInfo
toConnectInfo config = Redis.defaultConnectInfo
  { Redis.connectHost = config.host
  , Redis.connectPort = Redis.PortNumber (fromIntegral config.port)
  , Redis.connectDatabase = toInteger config.database
  , Redis.connectMaxConnections = config.maxConnections
  , Redis.connectMaxIdleTime = config.maxIdleTime
  }
