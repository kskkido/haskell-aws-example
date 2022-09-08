module Portfolio.Lib.Redis.Queries.Main
  ( get
  , set
  , setex
  ) where

import RIO hiding (set, exp)
import qualified RIO.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Database.Redis as Redis
import qualified Portfolio.Lib.Redis.Data.Query.Main as Query
import qualified Portfolio.Lib.Redis.Data.Config.Main as Config

get :: (Config.HasConfig a) => ByteString.ByteString -> Query.Query a (Maybe.Maybe ByteString.ByteString)
get bx = do
  Query.fromRedisEither $ Redis.get bx

set :: (Config.HasConfig a) => ByteString.ByteString -> ByteString.ByteString -> Query.Query a Redis.Status
set bx by = do
  Query.fromRedisEither $ Redis.set bx by

setex :: (Config.HasConfig a) => ByteString.ByteString -> ByteString.ByteString -> Integer -> Query.Query a Redis.Status
setex bx by exp = do
  Query.fromRedisEither $ Redis.setex bx exp by
