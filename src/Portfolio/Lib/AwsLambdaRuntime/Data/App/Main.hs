module Portfolio.Lib.AwsLambdaRuntime.Data.App.Main
  ( App
  , run
  , exec
  , fromMaybe
  , localM
  ) where

import RIO hiding (fromMaybe)
import qualified Data.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Except as ExceptT
import qualified Control.Monad.Reader as Reader

type App r m a = Reader.ReaderT r (Except.ExceptT String m) a

run ::  App r m a -> r -> m (Either String a)
run app env = Except.runExceptT $ Reader.runReaderT app env

exec :: r -> App r m a -> m (Either String a)
exec = flip run

fromMaybe :: Monad m => String -> Maybe.Maybe a -> App r m a
fromMaybe _ (Maybe.Just a) = pure a
fromMaybe cs Maybe.Nothing = Reader.ReaderT $ const $ ExceptT.throwE cs

localM :: Monad m => (r -> m r) -> App r m a -> App r m a
localM fn ma = do
  curr <- Reader.ask
  next <- (lift . lift) $ fn curr
  Reader.local (const next) ma
