module Portfolio.Applications.DeployStack.Data.CreateBucket.Main
  ( fromBucketName
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified Amazonka.S3
import qualified Amazonka.S3.CreateBucket
import qualified Amazonka.S3.Types
import qualified Amazonka.Env
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Applications.DeployStack.Data.App.Main as App

fromBucketName :: MonadIO m => String -> App.App m Amazonka.S3.CreateBucket.CreateBucket
fromBucketName bucketName = do
  amazonka <- Reader.asks $ \c -> c.amazonka
  pure
    ( ( Amazonka.S3.CreateBucket.newCreateBucket
        ( Amazonka.S3.BucketName $ Text.pack bucketName )
      ) &
      ( Amazonka.S3.CreateBucket.createBucket_createBucketConfiguration ?~
        ( ( Amazonka.S3.newCreateBucketConfiguration ) &
          ( Amazonka.S3.Types.createBucketConfiguration_locationConstraint ?~
            ( Amazonka.S3.LocationConstraint $ Amazonka.Env._envRegion amazonka )
          )
        )
      )
    )
