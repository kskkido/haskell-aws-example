module Portfolio.Applications.Client.Templates.Components.DrawingDetail.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Portfolio.Lib.Day.Main as Lib.Day
import qualified Portfolio.Lib.UTCTime.Main as Lib.UTCTime
import qualified Portfolio.Lib.Contentful.Data.RichText.Data.ContentNode.Main as ContentNode
import qualified Portfolio.Data.Drawing.Main as Drawing

eval :: Monad m => ContentNode.ContentNode -> [Lucid.Attribute] -> Lucid.HtmlT m ()
eval (ContentNode.DocumentNode node) attributes = do
  Lucid.div_ attributes do
    Foldable.foldMap eval node.content []
eval (ContentNode.ParagraphNode node) attributes = do
  Lucid.p_ attributes do
    Foldable.foldMap eval node.content []
eval (ContentNode.TextNode node) _ = do
  Lucid.toHtml (node.value)

render :: Monad m => Drawing.Drawing -> [Lucid.Attribute] -> Lucid.HtmlT m ()
render photograph attributes = do
  Lucid.div_ [] do
    Lucid.div_ [] do
      Lucid.span_ [Lucid.classes_ ["mr-4"]] do
        Lucid.toHtml ("title:" :: String)
      Lucid.span_ [] do
        Lucid.toHtml photograph.title
    Lucid.div_ [] do
      Lucid.span_ [Lucid.classes_ ["mr-4"]] do
        Lucid.toHtml ("date:" :: String)
      Lucid.span_ [] do
        Lucid.toHtml $ Lib.Day.toString $ Lib.UTCTime.toDay photograph.createdAt
    flip Foldable.foldMap photograph.caption $ \caption ->
      Lucid.div_ [] do
        Lucid.span_ [] do
          Lucid.toHtml ("description:" :: String)
        eval caption attributes


