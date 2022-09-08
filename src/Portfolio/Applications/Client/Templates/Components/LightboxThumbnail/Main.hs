module Portfolio.Applications.Client.Templates.Components.LightboxThumbnail.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Applications.Client.Data.LightboxSlide.Main as LightboxSlide

render :: (Lucid.ToHtml b, Monad m) => LightboxSlide.LightboxSlide b -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render slide attributes = do
  Lucid.div_ [Lucid.data_ "component" "lightbox_thumbnail", Lucid.data_ "lightbox_thumbnail_id" (Text.pack slide.id), Lucid.classes_ ["relative", "w-1/4", "shrink-0", "cursor-pointer"]] do
    Lucid.img_
      [ Lucid.src_ (Text.pack slide.image.src)
      , Lucid.classes_ ["absolute", "inset-0", "m-auto", "w-full", "h-full", "object-contain"]
      , Lucid.width_ $ Text.pack $ show slide.image.width <> "px"
      , Lucid.height_ $ Text.pack $ show slide.image.height <> "px"
      ]
