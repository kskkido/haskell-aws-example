module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.PutObject.Main
  ( fromBuild
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified Control.Monad.Reader as Reader
import qualified Network.Mime as Mime
import qualified Amazonka.S3
import qualified Amazonka.Data.Body
import qualified Amazonka.S3.PutObject
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.App.Main as App
import qualified Portfolio.Applications.StaticSiteGenerator.Data.Build.Main as StaticSiteGenerator.Build

fromBuild :: MonadIO m => StaticSiteGenerator.Build.Build -> App.App m [Amazonka.S3.PutObject.PutObject]
fromBuild build = do
  config <- Reader.ask
  forM build.files $ \relativeFilePath -> liftIO do
    let redirectFilePath = fromMaybe relativeFilePath do
          void $ Lib.FilePath.matchExtension "html" relativeFilePath
          pure $ Lib.FilePath.stripFileExtension relativeFilePath
        absoluteFilePath = Lib.FilePath.combine build.filePath relativeFilePath
    file <- Amazonka.Data.Body.Hashed . Amazonka.Data.Body.toHashed <$> Lib.ByteString.fromAnyFilePath absoluteFilePath
    pure
      ( ( file ) &
        ( Amazonka.S3.PutObject.newPutObject
          ( Amazonka.S3.BucketName config.pushBucketName )
          ( Amazonka.S3.ObjectKey $ Text.pack redirectFilePath )
        ) &
        ( Amazonka.S3.PutObject.putObject_contentType ?~
          ( fold $ Text.decodeUtf8' $ Mime.defaultMimeLookup $ Text.pack relativeFilePath )
        )
      )
