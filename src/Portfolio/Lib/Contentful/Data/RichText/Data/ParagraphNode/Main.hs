module Portfolio.Lib.Contentful.Data.RichText.Data.ParagraphNode.Main
  ( ParagraphNode(..)
  , fromResponseJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ParagraphNodeType.Main as ParagraphNodeType

data ParagraphNode a = ParagraphNode
  { content :: [a]
  , dataMap :: Aeson.Object
  , nodeType :: ParagraphNodeType.ParagraphNodeType
  }
  deriving (Eq, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (ParagraphNode a) where
  parseJSON = fromResponseJSON

fromResponseJSON :: Aeson.FromJSON a => Aeson.Value -> Aeson.Types.Parser (ParagraphNode a)
fromResponseJSON = Aeson.withObject "paragraphNode" $ \v -> ParagraphNode
  <$> v Aeson..: "content"
  <*> v Aeson..: "data"
  <*> v Aeson..: "nodeType"

