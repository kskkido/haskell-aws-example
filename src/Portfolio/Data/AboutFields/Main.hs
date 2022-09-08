module Portfolio.Data.AboutFields.Main
  ( AboutFields(..)
  , fromResponseJSON
  , fromItem
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.Item.Main as Item
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode

data AboutFields = AboutFields
  { title :: String
  , content :: ContentNode.ContentNode
  }
  deriving (Eq, Show)

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser AboutFields
fromResponseJSON = Aeson.withObject "aboutFields" $ \x -> AboutFields
  <$> x Aeson..: "title"
  <*> x Aeson..: "content"

fromItem :: Item.Item -> Aeson.Types.Parser AboutFields
fromItem item = fromResponseJSON item.fields
