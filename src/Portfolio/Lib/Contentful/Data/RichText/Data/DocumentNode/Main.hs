module Portfolio.Lib.Contentful.Data.RichText.Data.DocumentNode.Main
  ( DocumentNode(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.DocumentNodeType.Main as DocumentNodeType

data DocumentNode a = DocumentNode
  { content :: [a]
  , dataMap :: Aeson.Object
  , nodeType :: DocumentNodeType.DocumentNodeType
  }
  deriving (Eq, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (DocumentNode a) where
  parseJSON = fromResponseJSON

fromResponseJSON :: Aeson.FromJSON a => Aeson.Value -> Aeson.Types.Parser (DocumentNode a)
fromResponseJSON = Aeson.withObject "documentNode" $ \v -> DocumentNode
  <$> v Aeson..: "content"
  <*> v Aeson..: "data"
  <*> v Aeson..: "nodeType"
