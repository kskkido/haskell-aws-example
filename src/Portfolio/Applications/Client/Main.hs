module Portfolio.Applications.Client.Main
  ( main
  ) where

import RIO
import qualified RIO.Text.Lazy as Text.Lazy
import qualified Lucid
import qualified System.IO as IO
import qualified Control.Monad.Extra as Extra
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Localize.Locale
import qualified Portfolio.Applications.Client.Data.PageKey.Main as PageKey
import qualified Portfolio.Applications.Client.Templates.Pages.Home.Main as Pages.Home
import qualified Portfolio.Applications.Client.Templates.Pages.About.Main as Pages.About
import qualified Portfolio.Applications.Client.Templates.Pages.Translations.Main as Pages.Translations
import qualified Portfolio.Applications.Client.Templates.Pages.Photographs.Main as Pages.Photographs
import qualified Portfolio.Applications.Client.Templates.Pages.Drawings.Main as Pages.Drawings
import qualified Portfolio.Applications.Client.Data.PageContent.Main as PageContent
import qualified Portfolio.Applications.Client.Data.AppContext.Main as AppContext
import qualified Portfolio.Applications.Client.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Applications.Client.Data.Page.Main as Page

main :: AppConfig.AppConfig -> IO.IO [Page.Page Text.Lazy.Text]
main config = do
  contextEn <- AppContext.fromConfig config Localize.Locale.En
  contextJa <- AppContext.fromConfig config Localize.Locale.Ja
  flip foldMap [contextEn, contextJa] $ \context -> do
    entries <- Extra.fromMaybeM (throwString "Unable to get entries") $ MaybeT.runMaybeT do
      result <- liftIO (context.entriesRepository.get)
      either (pure mzero) pure result
    flip Reader.runReaderT context do
      sequence
        [ Reader.withReaderT (AppContext.toPageContext PageKey.Home) $ do
            let content = PageContent.fromEntries entries
            pageContext <- Reader.ask
            html <- Lucid.renderTextT $ Pages.Home.render content
            pure $ Page.Page
              { Page.context = pageContext
              , Page.html = html
              }
        , Reader.withReaderT (AppContext.toPageContext PageKey.About) $ do
            let content = PageContent.fromEntries entries
            pageContext <- Reader.ask
            html <- Lucid.renderTextT $ Pages.About.render content
            pure $ Page.Page
              { Page.context = pageContext
              , Page.html = html
              }
        , Reader.withReaderT (AppContext.toPageContext PageKey.Translations) $ do
            let content = PageContent.fromEntries entries
            pageContext <- Reader.ask
            html <- Lucid.renderTextT $ Pages.Translations.render content
            pure $ Page.Page
              { Page.context = pageContext
              , Page.html = html
              }
        , Reader.withReaderT (AppContext.toPageContext PageKey.Photographs) $ do
            let content = PageContent.fromEntries entries
            pageContext <- Reader.ask
            html <- Lucid.renderTextT $ Pages.Photographs.render content
            pure $ Page.Page
              { Page.context = pageContext
              , Page.html = html
              }
        , Reader.withReaderT (AppContext.toPageContext PageKey.Drawings) $ do
            let content = PageContent.fromEntries entries
            pageContext <- Reader.ask
            html <- Lucid.renderTextT $ Pages.Drawings.render content
            pure $ Page.Page
              { Page.context = pageContext
              , Page.html = html
              }
        ]

