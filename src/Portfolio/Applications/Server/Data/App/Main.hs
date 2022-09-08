module Portfolio.Applications.Server.Data.App.Main
  ( App
  ) where

import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Applications.Server.Data.AppContext.Main as AppContext

type App m a = Reader.ReaderT AppContext.AppContext m a
