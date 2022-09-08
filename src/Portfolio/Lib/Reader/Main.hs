module Portfolio.Lib.Reader.Main
  ( localM
  ) where

import RIO
import qualified Control.Monad.Reader as Reader

localM :: Monad m => (r -> m r) -> Reader.ReaderT r m a -> Reader.ReaderT r m a
localM fn ma = do
  curr <- Reader.ask
  next <- lift $ fn curr
  Reader.local (const next) ma
