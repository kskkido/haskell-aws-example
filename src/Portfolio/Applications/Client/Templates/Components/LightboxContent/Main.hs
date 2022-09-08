module Portfolio.Applications.Client.Templates.Components.LightboxContent.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute

render :: Monad m => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes html = do
  Lucid.div_ ([Lucid.classes_ []] `Lib.Lucid.Attribute.concat` attributes) html
