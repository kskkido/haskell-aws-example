module Portfolio.Lib.Either.Main
  ( liftFail
  , fromMaybe
  ) where

import RIO hiding (fromMaybe)
import qualified Control.Monad.Fail as Monad.Fail
import qualified Data.Maybe as Maybe

liftFail :: Monad.Fail.MonadFail m => m (Either String a) -> m a
liftFail ma = ma >>= either Monad.Fail.fail pure

fromMaybe :: b -> Maybe.Maybe a -> Either b a
fromMaybe b = Maybe.maybe (Left b) Right
