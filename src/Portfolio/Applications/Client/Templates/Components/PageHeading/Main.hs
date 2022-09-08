module Portfolio.Applications.Client.Templates.Components.PageHeading.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute

render :: Monad m => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes html = do
  Lucid.h1_ ([Lucid.classes_ ["mb-8", "text-xl"]] `Lib.Lucid.Attribute.concat` attributes) html
