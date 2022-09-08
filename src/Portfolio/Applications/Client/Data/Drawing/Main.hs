module Portfolio.Applications.Client.Data.Drawing.Main
  ( module Portfolio.Data.Drawing.Main
  , toImage
  , toLightboxSlide
  ) where

import RIO
import Portfolio.Data.Drawing.Main
import qualified Lucid
import qualified Portfolio.Applications.Client.Data.Image.Main as Image
import qualified Portfolio.Applications.Client.Data.LightboxSlide.Main as LightboxSlide
import qualified Portfolio.Applications.Client.Templates.Components.DrawingDetail.Main as Components.DrawingDetail

toImage :: Drawing -> Image.Image
toImage drawing = Image.Image
  { Image.src = drawing.url
  , Image.width = drawing.width
  , Image.height = drawing.height
  , Image.dataAttributes = toDataAttributes drawing
  }

toLightboxSlide :: Drawing -> LightboxSlide.LightboxSlide Drawing
toLightboxSlide drawing = LightboxSlide.LightboxSlide
  { LightboxSlide.id = drawing.id
  , LightboxSlide.image = toImage drawing
  , LightboxSlide.caption = drawing
  , LightboxSlide.dataAttributes = toDataAttributes drawing
  }

instance Lucid.ToHtml Drawing where
  toHtml x = Components.DrawingDetail.render x []
  toHtmlRaw x = Components.DrawingDetail.render x []
