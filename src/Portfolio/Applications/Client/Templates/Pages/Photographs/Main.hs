module Portfolio.Applications.Client.Templates.Pages.Photographs.Main
  ( render
  ) where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Localize.Main as Localize
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig
import qualified Portfolio.Applications.Client.Data.PageContent.Main as PageContent
import qualified Portfolio.Applications.Client.Data.Photograph.Main as Photograph
import qualified Portfolio.Applications.Client.Templates.Components.AppBody.Main as Components.AppBody
import qualified Portfolio.Applications.Client.Templates.Components.AppHead.Main as Components.AppHead
import qualified Portfolio.Applications.Client.Templates.Components.AppLayout.Main as Components.AppLayout
import qualified Portfolio.Applications.Client.Templates.Components.PageHeading.Main as Components.PageHeading
import qualified Portfolio.Applications.Client.Templates.Components.Lightbox.Main as Components.Lightbox
import qualified Portfolio.Applications.Client.Templates.Components.PhotographsByMonth.Main as Components.PhotographsByMonth

render :: (FilePathConfig.HasFilePathConfig a, PageMetadata.HasPageMetadata a, SiteMetadata.HasSiteMetadata a, PathConfig.HasPathConfig a, Localize.Config.HasConfig a, Monad m) => PageContent.PageContent -> Lucid.HtmlT (Reader.ReaderT a m) ()
render content = do
  pageMetadata <- Reader.asks PageMetadata.get
  title <- lift $ Localize.translateByConfig pageMetadata.title
  Lucid.doctypehtml_ do
    Components.AppHead.render [] do
      Lucid.script_ [Lucid.src_ "/scripts/pages/photographs/index.js", Lucid.defer_ ""] (Text.pack "")
    Components.AppBody.render [] do
      Components.AppLayout.render [] do
        Components.PageHeading.render [] $ Lucid.toHtml title
        let photographs =
              ( ( content.photographs ) &
                ( zip [(0 :: Int)..] ) &
                ( map \(i, photograph) -> photograph { Photograph.id = show i }) &
                ( List.sortBy Photograph.compareCreatedAt )
              )
            photographsByMonth =
              ( ( Photograph.groupByMonth photographs ) &
                ( List.sortBy (compare `on` fst) )
              )
        Components.PhotographsByMonth.render photographsByMonth []
        let slides = photographs <&> Photograph.toLightboxSlide
        Components.Lightbox.render slides [Lucid.id_ "lightbox_photographs", Lucid.classes_ ["hidden"]]

