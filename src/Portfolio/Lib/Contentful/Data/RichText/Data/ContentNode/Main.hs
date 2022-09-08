module Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main
  ( ContentNode(..)
  ) where

import RIO
import qualified Control.Monad
import qualified Data.Aeson as Aeson
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.DocumentNode.Main as DocumentNode
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ParagraphNode.Main as ParagraphNode
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.TextNode.Main as TextNode

data ContentNode =
    DocumentNode (DocumentNode.DocumentNode ContentNode)
  | ParagraphNode (ParagraphNode.ParagraphNode ContentNode)
  | TextNode TextNode.TextNode
  deriving (Eq, Show)

instance Aeson.FromJSON ContentNode where
  parseJSON v = do
    Control.Monad.msum
      [ DocumentNode.fromResponseJSON v <&> DocumentNode
      , ParagraphNode.fromResponseJSON v <&> ParagraphNode
      , TextNode.fromResponseJSON v <&> TextNode
      ]
