module Portfolio.Applications.Client.Templates.Components.LightboxHeader.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute

render :: (Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes = do
  Lucid.div_ ([Lucid.classes_ ["w-full", "flex", "flex-col", "justify-center"]] `Lib.Lucid.Attribute.concat` attributes) do
    Lucid.nav_ [Lucid.classes_ ["w-full"]] do
      Lucid.button_ [Lucid.data_ "component" "lightbox_close_button", Lucid.classes_ ["float-right"]] do
        Lucid.toHtml ("close" :: String)

