module Portfolio.Applications.Client.Data.Component.Main
  ( Component
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Applications.Client.Data.PageContext.Main as PageContext

type Component m a = a -> Lucid.HtmlT (Reader.ReaderT PageContext.PageContext m) ()
