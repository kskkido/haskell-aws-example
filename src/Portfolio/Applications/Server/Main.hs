module Portfolio.Applications.Server.Main
  ( main
  ) where

import RIO
import qualified RIO.Text.Lazy as Text.Lazy
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified Control.Monad.Extra as Extra
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Lucid
import qualified Web.Scotty as Scotty
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Middleware.Static as Static
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Localize.Locale
import qualified Portfolio.Data.Path.Main as Path
import qualified Portfolio.Applications.Server.Data.AppContext.Main as AppContext
import qualified Portfolio.Applications.Server.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Applications.Client.Data.PageKey.Main as PageKey
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.PageContent.Main as PageContent
import qualified Portfolio.Applications.Client.Templates.Pages.Home.Main as Pages.Home
import qualified Portfolio.Applications.Client.Templates.Pages.About.Main as Pages.About
import qualified Portfolio.Applications.Client.Templates.Pages.Translations.Main as Pages.Translations
import qualified Portfolio.Applications.Client.Templates.Pages.Photographs.Main as Pages.Photographs
import qualified Portfolio.Applications.Client.Templates.Pages.Drawings.Main as Pages.Drawings

main :: IO.IO ()
main = do
  config <- AppConfig.fromSystem >>= either (IO.Error.ioError . IO.Error.userError) return
  contextDefaultLocale <- AppContext.fromConfig config config.defaultLocale
  contextEn <- AppContext.fromConfig config Localize.Locale.En
  contextJa <- AppContext.fromConfig config Localize.Locale.Ja
  Scotty.scotty config.serverPort do
    Scotty.middleware Cors.simpleCors
    Scotty.middleware $ Static.staticPolicy $ Static.addBase "static/public"
    do
      let context = contextDefaultLocale
      do
        let page = PageKey.Home
            pageContext = AppContext.toPageContext page context
            path = Reader.runReader (PageMetadata.toPath pageContext.pageMetadata >>= Path.toLocalizedConfigPath) pageContext
        Scotty.get "/" do
          Scotty.redirect $ Text.Lazy.pack path
    flip Foldable.foldMap [contextEn, contextJa] $ \context -> do
      do
        let page = PageKey.Home
            pageContext = AppContext.toPageContext page context
            path = Reader.runReader (PageMetadata.toPath pageContext.pageMetadata >>= Path.toLocalizedConfigPath) pageContext
        Scotty.get (Path.toRoutePattern path) do
          html <- flip Scotty.rescue (const $ pure "") $ do
            flip Extra.fromMaybeM (liftIO $ context.pageRepository.get path) $ do
              result  <- liftIO (context.contentfulService.getEntries)
              content <- PageContent.fromEntries <$> either (Scotty.raise . Text.Lazy.pack) pure result
              html <- Reader.runReaderT (Lucid.renderTextT $ Pages.Home.render content) pageContext
              liftIO $ context.pageRepository.set path html 10
              pure html
          Scotty.html html
      do
        let page = PageKey.About
            pageContext = AppContext.toPageContext page context
            path = Reader.runReader (PageMetadata.toPath pageContext.pageMetadata >>= Path.toLocalizedConfigPath) pageContext
        Scotty.get (Path.toRoutePattern path) do
          html <- flip Scotty.rescue (const $ pure "") $ do
            flip Extra.fromMaybeM (liftIO $ context.pageRepository.get path) $ do
              result  <- liftIO (context.contentfulService.getEntries)
              content <- PageContent.fromEntries <$> either (Scotty.raise . Text.Lazy.pack) pure result
              html <- Reader.runReaderT (Lucid.renderTextT $ Pages.About.render content) pageContext
              liftIO $ context.pageRepository.set path html 10
              pure html
          Scotty.html html
      do
        let page = PageKey.Translations
            pageContext = AppContext.toPageContext page context
            path = Reader.runReader (PageMetadata.toPath pageContext.pageMetadata >>= Path.toLocalizedConfigPath) pageContext
        Scotty.get (Path.toRoutePattern path) do
          html <- flip Scotty.rescue (const $ pure "") $ do
            flip Extra.fromMaybeM (liftIO $ context.pageRepository.get path) $ do
              result  <- liftIO (context.contentfulService.getEntries)
              content <- PageContent.fromEntries <$> either (Scotty.raise . Text.Lazy.pack) pure result
              html <- Reader.runReaderT (Lucid.renderTextT $ Pages.Translations.render content) pageContext
              liftIO $ context.pageRepository.set path html 10
              pure html
          Scotty.html html
      do
        let page = PageKey.Drawings
            pageContext = AppContext.toPageContext page context
            path = Reader.runReader (PageMetadata.toPath pageContext.pageMetadata >>= Path.toLocalizedConfigPath) pageContext
        Scotty.get (Path.toRoutePattern path) do
          html <- flip Scotty.rescue (const $ pure "") $ do
            flip Extra.fromMaybeM (liftIO $ context.pageRepository.get path) $ do
              result  <- liftIO (context.contentfulService.getEntries)
              content <- PageContent.fromEntries <$> either (Scotty.raise . Text.Lazy.pack) pure result
              html <- Reader.runReaderT (Lucid.renderTextT $ Pages.Drawings.render content) pageContext
              liftIO $ context.pageRepository.set path html 10
              pure html
          Scotty.html html
      do
        let page = PageKey.Photographs
            pageContext = AppContext.toPageContext page context
            path = Reader.runReader (PageMetadata.toPath pageContext.pageMetadata >>= Path.toLocalizedConfigPath) pageContext
        Scotty.get (Path.toRoutePattern path) do
          html <- flip Scotty.rescue (const $ pure "") $ do
            flip Extra.fromMaybeM (liftIO $ context.pageRepository.get path) $ do
              result  <- liftIO (context.contentfulService.getEntries)
              content <- PageContent.fromEntries <$> either (Scotty.raise . Text.Lazy.pack) pure result
              html <- Reader.runReaderT (Lucid.renderTextT $ Pages.Photographs.render content) pageContext
              liftIO $ context.pageRepository.set path html 10
              pure html
          Scotty.html html
