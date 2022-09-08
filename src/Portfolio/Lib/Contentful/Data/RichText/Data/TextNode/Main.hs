module Portfolio.Lib.Contentful.Data.RichText.Data.TextNode.Main
  ( TextNode(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.TextNodeType.Main as TextNodeType

data TextNode = TextNode
  { value :: String
  , dataMap :: Aeson.Object
  , nodeType :: TextNodeType.TextNodeType
  }
  deriving (Eq, Show)

instance Aeson.FromJSON TextNode where
  parseJSON = fromResponseJSON

fromResponseJSON :: Aeson.Value -> Aeson.Types.Parser TextNode
fromResponseJSON = Aeson.withObject "textNode" $ \v -> TextNode
  <$> v Aeson..: "value"
  <*> v Aeson..: "data"
  <*> v Aeson..: "nodeType"

