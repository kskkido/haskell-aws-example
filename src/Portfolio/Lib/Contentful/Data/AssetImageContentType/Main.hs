module Portfolio.Lib.Contentful.Data.AssetImageContentType.Main
  ( AssetImageContentType(..)
  , unit
  , fromResponseJSON
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types

newtype AssetImageContentType = AssetImageContentType String
  deriving (Eq, Show)

unit :: AssetImageContentType
unit = AssetImageContentType "image/jpeg"

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AssetImageContentType
fromResponseJSON = Aeson.withText "assetImageContentType" $ \xs -> do
  Control.Monad.guard (xs == "image/jpeg")
  pure $ AssetImageContentType (Text.unpack xs)

