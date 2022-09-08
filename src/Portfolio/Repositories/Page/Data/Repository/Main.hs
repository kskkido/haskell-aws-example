module Portfolio.Repositories.Page.Data.Repository.Main
  ( Repository(..)
  , fromRedis
  ) where

import RIO hiding (exp)
import qualified RIO.Text.Lazy as Text.Lazy
import qualified System.IO as IO
import qualified Control.Retry as Retry
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.UTF8 as ByteString.UTF8
import qualified Portfolio.Lib.Handler.Main as Handler
import qualified Portfolio.Lib.Maybe.Main as Maybe
import qualified Portfolio.Lib.Redis.Data.Config.Main as Redis.Config
import qualified Portfolio.Lib.Redis.Data.Query.Main as Redis.Query
import qualified Portfolio.Lib.Redis.Queries.Main as Redis.Queries

data Repository = Repository
  { get :: String -> IO.IO (Maybe.Maybe Text.Lazy.Text)
  , set :: String -> Text.Lazy.Text -> Integer -> IO.IO ()
  }

fromRedis :: Redis.Config.HasConfig a => a -> Repository
fromRedis config = Repository
  { get = \key -> do
      Retry.recovering Retry.retryPolicyDefault [const $ Handler.const True] $ \sx -> do
        join . Maybe.fromEither <$> Redis.Query.exec config do
          Redis.Query.fromIO $ IO.print sx
          mval <- Redis.Queries.get (ByteString.UTF8.fromString key)
          pure (Text.Lazy.pack . ByteString.UTF8.toString <$> mval)
  , set = \key val exp -> do
      let bkey = ByteString.UTF8.fromString key
          bval = ByteString.UTF8.fromString $ Text.Lazy.unpack val
      Retry.recovering Retry.retryPolicyDefault [const $ Handler.const True] $ \sx -> do
        void $ Redis.Query.exec config do
          Redis.Query.fromIO $ IO.print sx
          Redis.Queries.setex bkey bval exp
  }
