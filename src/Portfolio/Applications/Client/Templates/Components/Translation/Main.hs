module Portfolio.Applications.Client.Templates.Components.Translation.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Time.Calendar as Time.Calendar
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Lib.Day.Main as Lib.Day
import qualified Portfolio.Lib.UTCTime.Main as Lib.UTCTime
import qualified Portfolio.Data.Translation.Main as Translation
import qualified Portfolio.Data.TranslationVariant.Main as TranslationVariant

render :: Monad m => Translation.Translation -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render translation attributes = do
    case Translation.toVariant translation of
      TranslationVariant.Linked tx -> do
        Lucid.a_ [Lucid.href_ $ Text.pack tx.link, Lucid.target_ "_blank"] do
          Lucid.div_ ([Lucid.classes_ []] `Lib.Lucid.Attribute.concat` attributes) do
            Lucid.span_ [Lucid.classes_ ["mr-4"]] do
              Lucid.toHtml $ Lib.Day.toString $ Lib.UTCTime.toDay tx.publishedAt
            Lucid.span_ [Lucid.classes_ ["underline"]] do
              Lucid.toHtml tx.title
      TranslationVariant.Static tx -> do
        Lucid.div_ ([Lucid.classes_ []] `Lib.Lucid.Attribute.concat` attributes) do
          Lucid.span_ [Lucid.classes_ ["mr-4"]] do
            Lucid.toHtml $ Lib.Day.toString $ Lib.UTCTime.toDay tx.publishedAt
          Lucid.span_ do
            Lucid.toHtml tx.title
