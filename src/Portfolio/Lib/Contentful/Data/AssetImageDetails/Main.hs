module Portfolio.Lib.Contentful.Data.AssetImageDetails.Main
  ( AssetImageDetails(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.AssetImageDimension.Main as AssetImageDimension

data AssetImageDetails = AssetImageDetails
  { image :: AssetImageDimension.AssetImageDimension
  , size :: Float
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImageDetails
fromResponseJSON = Aeson.withObject "assetImageDetails" $ \x -> AssetImageDetails
  <$> Aeson.Types.explicitParseField AssetImageDimension.fromResponseJSON x "image"
  <*> x Aeson..: "size"

