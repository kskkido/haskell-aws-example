module Portfolio.Applications.Client.Templates.Components.LightboxLayout.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Applications.Client.Templates.Components.LightboxHeader.Main as Components.LightboxHeader
import qualified Portfolio.Applications.Client.Templates.Components.LightboxContent.Main as Components.LightboxContent

render :: (Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes html = do
  Lucid.div_ ([Lucid.data_ "component" "lightbox_container", Lucid.classes_ ["fixed", "container", "z-50", "inset-0", "max-w-5xl", "w-screen", "m-auto", "h-full", "overflow-hidden", "flex", "flex-col"]] `Lib.Lucid.Attribute.concat` attributes) do
    Lucid.div_ [Lucid.data_ "component" "lightbox_backdrop", Lucid.classes_ ["fixed", "inset-0", "w-full", "h-full", "bg-white"]] mempty
    Components.LightboxHeader.render [Lucid.classes_ ["container", "z-10", "max-w-5xl", "inset-0", "mx-auto", "px-8", "h-32"]]
    Components.LightboxContent.render [Lucid.classes_ ["flex", "flex-col", "px-8", "pb-8", "mx-auto", "w-full", "grow", "relative"]] html

