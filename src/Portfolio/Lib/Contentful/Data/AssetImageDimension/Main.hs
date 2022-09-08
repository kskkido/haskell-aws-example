module Portfolio.Lib.Contentful.Data.AssetImageDimension.Main
  ( AssetImageDimension(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data AssetImageDimension = AssetImageDimension
  { height :: Float
  , width :: Float
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImageDimension
fromResponseJSON = Aeson.withObject "assetImageDimension" $ \x -> AssetImageDimension
  <$> x Aeson..: "height"
  <*> x Aeson..: "width"
