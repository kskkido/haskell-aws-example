module Portfolio.Applications.Client.Templates.Components.Drawing.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Data.Drawing.Main as Drawing

render :: Monad m => Drawing.Drawing -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render drawing attributes = do
  Lucid.img_
    ( [ Lucid.classes_ []
      , Lucid.src_ $ Text.pack drawing.url
      , Lucid.alt_ $ Text.pack drawing.title
      , Lucid.width_ $ Text.pack $ show drawing.width <> "px"
      , Lucid.height_ $ Text.pack $ show drawing.height <> "px"
      , Lucid.data_ "type" "drawing"
      ] <>
      Lib.Lucid.Attribute.fromDataAttributes (Drawing.toDataAttributes drawing) `Lib.Lucid.Attribute.concat`
      attributes
    )
