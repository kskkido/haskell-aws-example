module Portfolio.Applications.Client.Templates.Components.LightboxSlide.Main
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
  Lucid.div_ ([Lucid.data_ "component" "lightbox_slide", Lucid.data_ "lightbox_slide_id" (Text.pack slide.id), Lucid.classes_ ["absolute", "inset-0", "my-8", "flex", "flex-row", "align-center"]] `Lib.Lucid.Attribute.concat` attributes) do
    Lucid.div_ [Lucid.classes_ ["w-8/12", "h-full"]] do
      Lucid.img_
        [ Lucid.src_ (Text.pack slide.image.src)
        , Lucid.classes_ ["max-w-full", "max-h-full", "m-auto", "object-contain"]
        , Lucid.width_ $ Text.pack $ show slide.image.width <> "px"
        , Lucid.height_ $ Text.pack $ show slide.image.height <> "px"
        ]
    Lucid.div_ [Lucid.classes_ ["w-4/12", "h-full"]] do
      Lucid.toHtml slide.caption
