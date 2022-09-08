module Portfolio.Applications.Client.Templates.Components.Photograph.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Data.Photograph.Main as Photograph

render :: Monad m => Photograph.Photograph -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render photograph attributes = do
  Lucid.img_
    ( [ Lucid.classes_ []
      , Lucid.src_ $ Text.pack photograph.url
      , Lucid.alt_ $ Text.pack photograph.title
      , Lucid.width_ $ Text.pack $ show photograph.width <> "px"
      , Lucid.height_ $ Text.pack $ show photograph.height <> "px"
      , Lucid.data_ "type" "photograph"
      ] <>
      Lib.Lucid.Attribute.fromDataAttributes (Photograph.toDataAttributes photograph) `Lib.Lucid.Attribute.concat`
      attributes
    )
