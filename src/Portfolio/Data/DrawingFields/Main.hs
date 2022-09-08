module Portfolio.Data.DrawingFields.Main
  ( DrawingFields(..)
  , fromResponseJSON
  , fromItem
  ) where

import RIO
import qualified Data.Time.Clock as Time.Clock
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Maybe as Maybe
import qualified Portfolio.Lib.Contentful.Data.Item.Main as Item
import qualified Portfolio.Lib.Contentful.Data.ItemFieldsImage.Main as ItemFieldsImage
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode

data DrawingFields = DrawingFields
  { image :: ItemFieldsImage.ItemFieldsImage
  , title :: String
  , createdAt :: Time.Clock.UTCTime
  , caption :: Maybe.Maybe ContentNode.ContentNode
  }
  deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser DrawingFields
fromResponseJSON = Aeson.withObject "drawingFields" $ \x -> DrawingFields
  <$> Aeson.Types.explicitParseField ItemFieldsImage.fromResponseJSON x "image"
  <*> x Aeson..: "title"
  <*> x Aeson..: "createdAt"
  <*> x Aeson..:? "caption"

fromItem :: Item.Item -> Aeson.Types.Parser DrawingFields
fromItem item = fromResponseJSON item.fields
