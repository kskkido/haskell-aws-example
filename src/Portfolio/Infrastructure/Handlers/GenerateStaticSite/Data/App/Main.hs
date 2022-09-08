module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.App.Main
  ( App
  , run
  , exec
  ) where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppContext.Main as AppContext

type App m a = Reader.ReaderT AppContext.AppContext m a

run :: App m a -> AppContext.AppContext -> m a
run = Reader.runReaderT

exec :: AppContext.AppContext -> App m a -> m a
exec = flip run
