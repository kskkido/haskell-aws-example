module Portfolio.Applications.Client.Data.Photograph.Main
  ( module Portfolio.Data.Photograph.Main
  , toImage
  , toLightboxSlide
  ) where

import RIO
import qualified Lucid
import Portfolio.Data.Photograph.Main
import qualified Portfolio.Applications.Client.Data.Image.Main as Image
import qualified Portfolio.Applications.Client.Data.LightboxSlide.Main as LightboxSlide
import qualified Portfolio.Applications.Client.Templates.Components.PhotographDetail.Main as Components.PhotographDetail

toImage :: Photograph -> Image.Image
toImage photograph = Image.Image
  { Image.src = photograph.url
  , Image.width = photograph.width
  , Image.height = photograph.height
  , Image.dataAttributes = toDataAttributes photograph
  }

toLightboxSlide :: Photograph -> LightboxSlide.LightboxSlide Photograph
toLightboxSlide photograph = LightboxSlide.LightboxSlide
  { LightboxSlide.id = photograph.id
  , LightboxSlide.image = toImage photograph
  , LightboxSlide.caption = photograph
  , LightboxSlide.dataAttributes = toDataAttributes photograph
  }

instance Lucid.ToHtml Photograph where
  toHtml x = Components.PhotographDetail.render x []
  toHtmlRaw x = Components.PhotographDetail.render x []
