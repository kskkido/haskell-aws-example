module Portfolio.Lib.Contentful.Data.AssetImage.Main
  ( AssetImage(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.AssetImageFields.Main as AssetImageFields
import qualified Portfolio.Lib.Contentful.Data.AssetImageSys.Main as AssetImageSys

data AssetImage = AssetImage
  { fields :: AssetImageFields.AssetImageFields
  , sys :: AssetImageSys.AssetImageSys
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImage
fromResponseJSON = Aeson.withObject "assetImageFile" $ \x -> AssetImage
  <$> Aeson.Types.explicitParseField AssetImageFields.fromResponseJSON x "fields"
  <*> Aeson.Types.explicitParseField AssetImageSys.fromResponseJSON x "sys"

