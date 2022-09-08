module Portfolio.Applications.StaticSiteGenerator.Data.File.Main
  ( File(..)
  , fromPage
  , toFilePath
  , toRelativeFile
  ) where

import RIO
import qualified RIO.Text.Lazy as Text.Lazy
import qualified System.IO as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Portfolio.Data.Path.Main as Path
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.Directory.Main as Lib.Directory
import qualified Portfolio.Applications.Client.Data.PageMetadata.Main as PageMetadata
import qualified Portfolio.Applications.Client.Data.Page.Main as Page

newtype File = File IO.FilePath

fromPage :: String -> Page.Page Text.Lazy.Text -> IO.IO File
fromPage basePath page = flip Reader.runReaderT page.context do
  pageContext <- Reader.ask
  path <-
    ( PageMetadata.toPath pageContext.pageMetadata >>=
      Path.toLocalizedConfigPath >>=
      Path.prepend basePath >>=
      Path.setExtension "html"
    )
  liftIO $ IO.print path
  liftIO $ Lib.Directory.writeFile path (Text.Lazy.unpack page.html)
  pure $ File path

toFilePath :: File -> IO.FilePath
toFilePath (File filePath) = filePath

toRelativeFile :: IO.FilePath -> File -> File
toRelativeFile directory (File filePath) = File (Lib.FilePath.toRelativePath directory filePath)
