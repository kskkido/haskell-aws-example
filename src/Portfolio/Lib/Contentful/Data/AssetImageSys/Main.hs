module Portfolio.Lib.Contentful.Data.AssetImageSys.Main
  ( AssetImageSys(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

data AssetImageSys = AssetImageSys
  { id :: String
  } deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImageSys
fromResponseJSON = Aeson.withObject "assetImageSys" $ \x -> AssetImageSys
  <$> x Aeson..: "id"

