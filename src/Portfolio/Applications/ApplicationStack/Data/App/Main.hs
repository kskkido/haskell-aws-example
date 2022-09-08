module Portfolio.Applications.ApplicationStack.Data.App.Main
  ( App
  , run
  , exec
  ) where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Applications.ApplicationStack.Data.AppConfig.Main as AppConfig

type App m a = Reader.ReaderT AppConfig.AppConfig m a

run :: App m a -> AppConfig.AppConfig -> m a
run = Reader.runReaderT

exec :: AppConfig.AppConfig -> App m a -> m a
exec = flip run
