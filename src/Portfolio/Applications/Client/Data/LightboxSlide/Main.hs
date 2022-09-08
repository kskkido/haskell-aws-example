module Portfolio.Applications.Client.Data.LightboxSlide.Main
  ( LightboxSlide(..)
  ) where

import RIO
import qualified Portfolio.Applications.Client.Data.Image.Main as Image
import qualified Portfolio.Lib.Lucid.Data.DataAttributes.Main as Lib.Lucid.DataAttributes

data LightboxSlide a = LightboxSlide
  { id :: String
  , image :: Image.Image
  , caption :: a
  , dataAttributes :: Lib.Lucid.DataAttributes.DataAttributes
  }
  deriving (Eq, Show)
