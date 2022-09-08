module Portfolio.Applications.Client.Templates.Components.Lightbox.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Applications.Client.Data.LightboxSlide.Main as LightboxSlide
import qualified Portfolio.Applications.Client.Templates.Components.LightboxLayout.Main as Components.LightboxLayout
import qualified Portfolio.Applications.Client.Templates.Components.LightboxSlide.Main as Components.LightboxSlide
import qualified Portfolio.Applications.Client.Templates.Components.LightboxThumbnail.Main as Components.LightboxThumbnail

render :: (Lucid.ToHtml b, Monad m) => [LightboxSlide.LightboxSlide b] -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render slides attributes = do
  Components.LightboxLayout.render ([Lucid.classes_ []] `Lib.Lucid.Attribute.concat` attributes) do
    Lucid.div_ [Lucid.data_ "component" "lightbox_slides", Lucid.classes_ ["relative", "h-4/5"]] do
      flip Foldable.foldMap slides $ \slide -> do
        Components.LightboxSlide.render slide []
    Lucid.div_ [Lucid.data_ "component" "lightbox_thumbnails", Lucid.classes_ ["relative", "w-full", "h-1/5", "m-auto", "flex", "flex-row", "gap-x-1", "overflow-x-scroll"]] do
      flip Foldable.foldMap slides $ \slide -> do
        Components.LightboxThumbnail.render slide []
