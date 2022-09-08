module Portfolio.Lib.Redis.Data.Query.Main
  ( Query
  , run
  , exec
  , fromIO
  , fromRedis
  , fromEither
  , fromRedisEither
  ) where

import RIO hiding (fromEither)
import qualified System.IO as IO
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Database.Redis as Redis
import qualified Portfolio.Lib.Redis.Data.Config.Main as Config

type Query a b = Reader.ReaderT a (Except.ExceptT String Redis.Redis) b

run :: (Config.HasConfig a) => Query a b -> a -> IO.IO (Either String b)
run qx env = do
  let action = Except.runExceptT $ Reader.runReaderT qx env
  connection <- Redis.connect $ Config.toConnectInfo $ Config.get env
  Redis.runRedis connection action

exec :: (Config.HasConfig a) => a -> Query a b -> IO.IO (Either String b)
exec = flip run

fromIO :: IO.IO b -> Query a b
fromIO = liftIO

fromEither :: Either String b -> Query a b
fromEither = lift . Except.ExceptT . pure

fromRedis :: Redis.Redis b -> Query a b
fromRedis action = lift . Except.ExceptT $ pure <$> action

fromRedisEither :: Redis.Redis (Either b c) -> Query a c
fromRedisEither action = lift . Except.ExceptT $ first (const "Redis error") <$> action
