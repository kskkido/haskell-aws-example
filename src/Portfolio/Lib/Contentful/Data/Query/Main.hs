module Portfolio.Lib.Contentful.Data.Query.Main
  ( Query
  , run
  , exec
  , fromEither
  ) where

import RIO hiding (fromEither)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified System.IO as IO
import qualified Portfolio.Lib.Contentful.Data.Config.Main as Config

type Query a b = Reader.ReaderT a (Except.ExceptT String IO.IO) b

run :: (Config.HasConfig a) => Query a b -> a -> IO.IO (Either String b)
run qx env = Except.runExceptT $ Reader.runReaderT qx env

exec :: (Config.HasConfig a) => a -> Query a b -> IO.IO (Either String b)
exec = flip run

fromEither :: (Config.HasConfig a) => Either String b -> Query a b
fromEither = lift . Except.ExceptT . pure

