module Portfolio.Applications.ApplicationStack.Data.PutObject.Main
  ( fromRelativeFilePath
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import qualified Network.Mime as Mime
import qualified Amazonka.S3
import qualified Amazonka.Data.Body
import qualified Amazonka.S3.PutObject
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Applications.ApplicationStack.Data.App.Main as App

fromRelativeFilePath :: MonadIO m => String -> FilePath.FilePath -> App.App m Amazonka.S3.PutObject.PutObject
fromRelativeFilePath bucketName filePath = do
  absoluteFilePath <- liftIO $ Lib.FilePath.toAbsolutePath filePath
  object <-
    ( liftIO $ IO.readFile absoluteFilePath ) <&>
    ( Amazonka.Data.Body.Hashed . Amazonka.Data.Body.toHashed . Lib.ByteString.fromUtf8String ) <&>
    ( Amazonka.S3.PutObject.newPutObject
      ( Amazonka.S3.BucketName $ Text.pack bucketName )
      ( Amazonka.S3.ObjectKey $ Text.pack filePath )
    ) <&>
    ( Amazonka.S3.PutObject.putObject_contentType ?~
      ( fold $ Text.decodeUtf8' $ Mime.defaultMimeLookup $ Text.pack filePath )
    )
  pure object
