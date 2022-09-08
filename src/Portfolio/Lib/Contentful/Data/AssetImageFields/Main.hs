module Portfolio.Lib.Contentful.Data.AssetImageFields.Main
  ( AssetImageFields(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Portfolio.Lib.Contentful.Data.AssetImageFile.Main as AssetImageFile

data AssetImageFields = AssetImageFields
  { description :: Maybe.Maybe String
  , title :: String
  , file :: AssetImageFile.AssetImageFile
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImageFields
fromResponseJSON = Aeson.withObject "assetImageFields" $ \x -> AssetImageFields
  <$> x Aeson..:? "description"
  <*> x Aeson..: "title"
  <*> Aeson.Types.explicitParseField AssetImageFile.fromResponseJSON x "file"

