module Portfolio.Applications.Client.Templates.Components.AppHead.Main
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Localize.Main as Localize
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.FilePathConfig.Main as FilePathConfig
import qualified Portfolio.Applications.Client.Templates.Components.AppStyles.Main as Components.AppStyles
import qualified Portfolio.Applications.Client.Templates.Components.AppScripts.Main as Components.AppScripts

render :: (Localize.Config.HasConfig a, PageMetadata.HasPageMetadata a, FilePathConfig.HasFilePathConfig a, Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes html = do
  pageMetadata <- Reader.asks PageMetadata.get
  title <- lift $ Localize.translateByConfig pageMetadata.title
  Lucid.head_ attributes do
    Lucid.title_ $ Lucid.toHtml title
    Lucid.meta_ [Lucid.charset_ "UTF-8"]
    Components.AppStyles.render
    Components.AppScripts.render
    html

