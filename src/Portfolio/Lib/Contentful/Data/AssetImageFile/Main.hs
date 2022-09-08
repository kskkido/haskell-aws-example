module Portfolio.Lib.Contentful.Data.AssetImageFile.Main
  ( AssetImageFile(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.AssetImageDetails.Main as AssetImageDetails
import qualified Portfolio.Lib.Contentful.Data.AssetImageContentType.Main as AssetImageContentType

data AssetImageFile = AssetImageFile
  { contentType :: AssetImageContentType.AssetImageContentType
  , details :: AssetImageDetails.AssetImageDetails
  , fileName :: String
  , url :: String
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImageFile
fromResponseJSON = Aeson.withObject "assetImageFile" $ \x -> AssetImageFile
  <$> Aeson.Types.explicitParseField AssetImageContentType.fromResponseJSON x "contentType"
  <*> Aeson.Types.explicitParseField AssetImageDetails.fromResponseJSON x "details"
  <*> x Aeson..: "fileName"
  <*> x Aeson..: "url"

