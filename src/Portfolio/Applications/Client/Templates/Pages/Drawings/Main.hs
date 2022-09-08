module Portfolio.Applications.Client.Templates.Pages.Drawings.Main
  ( render
  ) where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Portfolio.Lib.Localize.Main as Localize
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig
import qualified Portfolio.Applications.Client.Data.PageContent.Main as PageContent
import qualified Portfolio.Applications.Client.Data.Drawing.Main as Drawing
import qualified Portfolio.Applications.Client.Templates.Components.AppBody.Main as Components.AppBody
import qualified Portfolio.Applications.Client.Templates.Components.AppHead.Main as Components.AppHead
import qualified Portfolio.Applications.Client.Templates.Components.AppLayout.Main as Components.AppLayout
import qualified Portfolio.Applications.Client.Templates.Components.PageHeading.Main as Components.PageHeading
import qualified Portfolio.Applications.Client.Templates.Components.Lightbox.Main as Components.Lightbox
import qualified Portfolio.Applications.Client.Templates.Components.DrawingsByMonth.Main as Components.DrawingsByMonth

render :: (FilePathConfig.HasFilePathConfig a, PageMetadata.HasPageMetadata a, SiteMetadata.HasSiteMetadata a, PathConfig.HasPathConfig a, Localize.Config.HasConfig a, Monad m) => PageContent.PageContent -> Lucid.HtmlT (Reader.ReaderT a m) ()
render content = do
  pageMetadata <- Reader.asks PageMetadata.get
  title <- lift $ Localize.translateByConfig pageMetadata.title
  Lucid.doctypehtml_ do
    Components.AppHead.render [] do
      Lucid.script_ [Lucid.src_ "/scripts/pages/drawings/index.js", Lucid.defer_ ""] (Text.pack "")
    Components.AppBody.render [] do
      Components.AppLayout.render [] do
        Components.PageHeading.render [] $ Lucid.toHtml title
        let drawings =
              ( ( content.drawings ) &
                ( zip [(0 :: Int)..] ) &
                ( map \(i, drawing) -> drawing { Drawing.id = show i }) &
                ( List.sortBy Drawing.compareCreatedAt )
              )
            drawingsByMonth =
              ( ( Drawing.groupByMonth drawings ) &
                ( List.sortBy (compare `on` fst) )
              )
        Components.DrawingsByMonth.render drawingsByMonth []
        let slides = drawings <&> Drawing.toLightboxSlide
        Components.Lightbox.render slides [Lucid.id_ "lightbox_drawings", Lucid.classes_ ["hidden"]]

