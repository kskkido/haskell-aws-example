module Portfolio.Applications.Client.Templates.Components.AppLayout.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata
import qualified Portfolio.Applications.Client.Templates.Components.AppHeader.Main as Components.AppHeader
import qualified Portfolio.Applications.Client.Templates.Components.AppSidebar.Main as Components.AppSidebar
import qualified Portfolio.Applications.Client.Templates.Components.AppContent.Main as Components.AppContent

render :: (PageMetadata.HasPageMetadata a, SiteMetadata.HasSiteMetadata a, PathConfig.HasPathConfig a, Localize.Config.HasConfig a, Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes html = do
  Lucid.div_ ([Lucid.classes_ ["container", "max-w-5xl", "mx-auto", "min-h-screen", "flex", "flex-col"]] `Lib.Lucid.Attribute.concat` attributes) do
    -- Components.AppHeader.render [Lucid.classes_ ["fixed", "sm:relative", "inset-0", "pb-8", "sm:pb-16", "grow-0"]]
    Components.AppHeader.render [Lucid.classes_ ["container", "fixed", "z-30", "max-w-5xl", "inset-0", "mx-auto", "px-8", "h-32"]]
    Lucid.div_ [Lucid.classes_ ["relative", "pt-32", "pb-8", "px-8", "flex", "flex-row", "flex-wrap", "grow"]] do
      Components.AppSidebar.render [Lucid.classes_ ["container", "fixed", "top-32", "w-56", "h-fit"]]
      Components.AppContent.render [Lucid.classes_ ["ml-56", "w-full", "relative"]] html

