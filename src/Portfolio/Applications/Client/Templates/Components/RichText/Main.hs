module Portfolio.Applications.Client.Templates.Components.RichText.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode

eval :: Monad m => ContentNode.ContentNode -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
eval (ContentNode.DocumentNode node) attributes = do
  Lucid.div_ attributes do
    Foldable.foldMap eval node.content []
eval (ContentNode.ParagraphNode node) attributes = do
  Lucid.p_ attributes do
    Foldable.foldMap eval node.content []
eval (ContentNode.TextNode node) _ = do
  Lucid.toHtml (node.value)

render :: Monad m => ContentNode.ContentNode -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render contentNode attributes = do
  eval contentNode attributes
