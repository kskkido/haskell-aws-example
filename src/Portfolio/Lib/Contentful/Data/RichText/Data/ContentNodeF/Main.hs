module Portfolio.Lib.Contentful.Data.RichText.Data.ContentNodeF.Main
  ( ContentNodeF(..)
  ) where

import RIO
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.DocumentNode.Main as DocumentNode
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ParagraphNode.Main as ParagraphNode
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.TextNode.Main as TextNode

data ContentNodeF a =
    DocumentNode (DocumentNode.DocumentNode a)
  | ParagraphNode (ParagraphNode.ParagraphNode a)
  | TextNode TextNode.TextNode
  deriving (Eq, Show)

instance Aeson.FromJSON a => Aeson.FromJSON (ContentNodeF a) where
  parseJSON v = do
    Control.Monad.msum
      [ DocumentNode.fromResponseJSON v <&> DocumentNode
      , ParagraphNode.fromResponseJSON v <&> ParagraphNode
      , TextNode.fromResponseJSON v <&> TextNode
      ]
