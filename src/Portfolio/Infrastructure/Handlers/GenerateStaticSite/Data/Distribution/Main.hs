module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.Distribution.Main
  ( invalidate
  , invalidateAllFiles
  ) where

import RIO
import qualified RIO.Text as Text
import Control.Lens
import qualified Control.Monad.Reader as Reader
import qualified Network.Mime as Mime
import qualified Amazonka.S3
import qualified Amazonka.Data.Body
import qualified Amazonka.CloudFront.Types.Paths
import qualified Amazonka.CloudFront.Types.InvalidationBatch
import qualified Amazonka.CloudFront.CreateInvalidation
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.App.Main as App

invalidate :: MonadIO m => Amazonka.CloudFront.Types.InvalidationBatch.InvalidationBatch -> App.App m Amazonka.CloudFront.CreateInvalidation.CreateInvalidation
invalidate batch = do
  config <- Reader.ask
  pure $ Amazonka.CloudFront.CreateInvalidation.newCreateInvalidation config.distributionId batch

invalidateAllFiles :: MonadIO m => App.App m Amazonka.CloudFront.CreateInvalidation.CreateInvalidation
invalidateAllFiles = invalidate $ Amazonka.CloudFront.Types.InvalidationBatch.newInvalidationBatch
  ( ( Amazonka.CloudFront.Types.Paths.newPaths 1 ) &
    ( Amazonka.CloudFront.Types.Paths.paths_items ?~
        ["/*"]
    )
  )
  "invalidateAllFiles"
