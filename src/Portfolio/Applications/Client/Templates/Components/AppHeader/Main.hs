module Portfolio.Applications.Client.Templates.Components.AppHeader.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Lib.Localize.Main as Localize
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Localize.Locale
import qualified Portfolio.Data.PathInfo.Main as PathInfo
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageKey.Main as PageKey
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata

render :: (SiteMetadata.HasSiteMetadata a, PageMetadata.HasPageMetadata a, PathConfig.HasPathConfig a, Localize.Config.HasConfig a, Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes = do
  localizeConfig <- lift $ Reader.asks Localize.Config.get
  siteMetadata <- lift $ Reader.asks SiteMetadata.get
  pageMetadata <- lift $ Reader.asks PageMetadata.get
  let locale = Localize.Locale.toggle localizeConfig.locale
  Lucid.header_ ([Lucid.classes_ ["w-full", "flex", "flex-col", "justify-center"]] `Lib.Lucid.Attribute.concat` attributes) do
    Lucid.nav_ [Lucid.classes_ ["flex", "flex-row", "justify-between"]] do
      Lucid.ul_ [Lucid.classes_ ["flex", "flex-row", "grow-1"]] do
        homeMetadata <- lift $ PageKey.toPageMetadata PageKey.Home
        homePath <- lift $ PathInfo.toLocalizedPath localizeConfig.locale homeMetadata.pathInfo
        Lucid.a_ [Lucid.href_ $ Text.pack homePath] do
          localized <- lift $ Localize.translateByConfig siteMetadata.home.title
          Lucid.toHtml localized
      Lucid.ul_ [Lucid.classes_ ["flex", "flex-row", "grow-1"]] do
        path <- lift $ PathInfo.toLocalizedPath locale pageMetadata.pathInfo
        Lucid.a_ [Lucid.href_ $ Text.pack path] do
          localized <- lift $ Localize.translateByConfig ("locale." <> show locale)
          Lucid.toHtml localized

