module Portfolio.Applications.DeployStack.Data.LambdaSource.Main
  ( upload
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified Control.Exception.Lens as Exception.Lens
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import qualified Amazonka
import qualified Amazonka.Data.Body
import qualified Amazonka.Env
import qualified Amazonka.Error
import qualified Amazonka.S3
import qualified Amazonka.S3.CreateBucket
import qualified Amazonka.S3.Types
import qualified Amazonka.S3.PutObject
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Applications.DeployStack.Data.App.Main as App

upload :: MonadIO m => App.App m ()
upload = do
  config <- Reader.ask
  source <- liftIO do
    absoluteFilePath <- Lib.FilePath.toAbsolutePath config.lambdaFilePath
    Lib.ByteString.fromFilePath absoluteFilePath
  void $ liftIO $ Amazonka.runResourceT do
    let createSourceBucket =
          ( ( Amazonka.S3.CreateBucket.newCreateBucket
                      ( Amazonka.S3.BucketName $ Text.pack config.lambdaBucketName )
                    ) &
                    ( Amazonka.S3.CreateBucket.createBucket_createBucketConfiguration ?~
                      ( ( Amazonka.S3.newCreateBucketConfiguration ) &
                        ( Amazonka.S3.Types.createBucketConfiguration_locationConstraint ?~
                          ( Amazonka.S3.LocationConstraint $ Amazonka.Env._envRegion config.amazonka )
                        )
                      )
                    )
                  )
        putSourceObject =
          ( Amazonka.Data.Body.Hashed $ Amazonka.Data.Body.toHashed source ) &
          ( Amazonka.S3.PutObject.newPutObject
            ( Amazonka.S3.BucketName $ Text.pack config.lambdaBucketName )
            ( Amazonka.S3.ObjectKey $ Text.pack config.lambdaFilePath )
          )
    Exception.Lens.trying Amazonka.S3.Types._BucketAlreadyOwnedByYou do
      Amazonka.send config.amazonka createSourceBucket
    Amazonka.send config.amazonka putSourceObject
