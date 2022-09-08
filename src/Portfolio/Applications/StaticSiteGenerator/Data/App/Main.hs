module Portfolio.Applications.StaticSiteGenerator.Data.App.Main
  ( App
  ) where

import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppContext.Main as AppContext

type App m a = Reader.ReaderT AppContext.AppContext m a
