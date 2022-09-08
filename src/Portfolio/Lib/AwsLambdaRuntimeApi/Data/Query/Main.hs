module Portfolio.Lib.AwsLambdaRuntimeApi.Data.Query.Main
  ( Query
  , run
  , exec
  , fromEither
  ) where

import RIO hiding (fromEither)
import qualified System.IO as IO
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader

type Query a b = Reader.ReaderT a (Except.ExceptT String IO.IO) b

run :: Query a b -> a -> IO.IO (Either String b)
run qx env = Except.runExceptT $ Reader.runReaderT qx env

exec :: a -> Query a b -> IO.IO (Either String b)
exec = flip run

fromEither :: Either String b -> Query a b
fromEither = lift . Except.ExceptT . pure

