module Portfolio.Applications.Client.Templates.Components.AppSidebar.Main
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import qualified Portfolio.Lib.Lucid.Attribute.Main as Lib.Lucid.Attribute
import qualified Portfolio.Lib.Localize.Main as Localize
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Data.PathInfo.Main as PathInfo
import qualified Portfolio.Data.PathConfig.Main as PathConfig
import qualified Portfolio.Applications.Client.Data.PageKey.Main as PageKey
import qualified Portfolio.Applications.Client.Data.SiteMetadata.Main as SiteMetadata

render :: (Localize.Config.HasConfig a, PathConfig.HasPathConfig a, SiteMetadata.HasSiteMetadata a, Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes = do
  routes <- lift
    ( ( [ PageKey.About
        , PageKey.Translations
        , PageKey.Photographs
        , PageKey.Drawings
        ]
      ) &
      ( Traversable.traverse $ \page -> do
          locale <- Reader.asks $ Localize.Config.locale . Localize.Config.get
          metadata <- PageKey.toPageMetadata page
          title <- Localize.translateByConfig metadata.title
          path <- PathInfo.toLocalizedPath locale metadata.pathInfo
          pure (title, Text.pack path)
      )
    )
  Lucid.aside_ ([] `Lib.Lucid.Attribute.concat` attributes) do
    Lucid.div_ [Lucid.classes_ ["overflow-y-auto"]] do
      Lucid.ul_ [Lucid.classes_ []] do
        flip Foldable.foldMap routes $ \(name, path) -> do
            Lucid.li_ [Lucid.classes_ ["pb-8"]] do
              Lucid.a_ [Lucid.href_ path, Lucid.classes_ ["flex", "items-center", "text-base", "font-normal"]] do
                Lucid.toHtml name
