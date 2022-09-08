module Portfolio.Lib.Generator.Data.App.Main
  ( App
  , runApp
  ) where

import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Generator.Data.AppContext.Main as AppContext

type App m a = Reader.ReaderT AppContext.AppContext m a

runApp :: App m a -> AppContext.AppContext -> m a
runApp = Reader.runReaderT

-- at "" <> build "page"
--
