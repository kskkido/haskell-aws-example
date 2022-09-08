module Portfolio.Applications.Client.Templates.Pages.Home.Main
  ( render
  ) where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Data.PathInfo.Main as PathInfo
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig
import qualified Portfolio.Applications.Client.Data.PageKey.Main as PageKey
import qualified Portfolio.Applications.Client.Data.PageContent.Main as PageContent
import qualified Portfolio.Applications.Client.Templates.Components.AppBody.Main as Components.AppBody
import qualified Portfolio.Applications.Client.Templates.Components.AppHead.Main as Components.AppHead
import qualified Portfolio.Applications.Client.Templates.Components.AppLayout.Main as Components.AppLayout
import qualified Portfolio.Applications.Client.Templates.Components.Photograph.Main as Components.Photograph

render :: (FilePathConfig.HasFilePathConfig a, PageMetadata.HasPageMetadata a, SiteMetadata.HasSiteMetadata a, PathConfig.HasPathConfig a, Localize.Config.HasConfig a, Monad m) => PageContent.PageContent -> Lucid.HtmlT (Reader.ReaderT a m) ()
render content = do
  Lucid.doctypehtml_ do
    Components.AppHead.render [] mempty
    Components.AppBody.render [] do
      Components.AppLayout.render [] do
        Maybe.fromMaybe mempty $ do
          photograph <- List.headMaybe content.photographs
          pure $ do
            path <- lift do
              locale <- Reader.asks $ Localize.Config.locale . Localize.Config.get
              metadata <- PageKey.toPageMetadata PageKey.Photographs
              PathInfo.toLocalizedPath locale metadata.pathInfo
            Lucid.a_ [Lucid.href_ $ Text.pack path] do
              Components.Photograph.render photograph [Lucid.classes_ ["absolute", "bottom-0", "right-0"], Lucid.width_ "450px", Lucid.height_ "600px"]

