module Portfolio.Lib.Monad.Main
  ( mfold
  ) where

import RIO
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable

mfold :: (Monad.MonadPlus m, Foldable t) => t a -> m a
mfold  = Monad.msum . map pure . Foldable.toList
