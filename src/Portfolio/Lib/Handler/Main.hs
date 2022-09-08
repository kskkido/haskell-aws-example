module Portfolio.Lib.Handler.Main
  ( const
  ) where

import RIO hiding (const)
import qualified Control.Monad.Catch as Catch
import qualified Control.Exception as Exception

const :: Monad m => a -> Catch.Handler m a
const x = Catch.Handler $ \(_ :: Exception.SomeException) -> return x
