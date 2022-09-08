module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppRemoteConfig.Main
  ( fromRemoteConfig
  ) where

import RIO hiding ((^.))
import qualified RIO.Text as Text
import Control.Lens
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified System.Environment as Environment
import qualified Data.Aeson as Aeson
import qualified Amazonka
import qualified Amazonka.SSM.GetParameter
import qualified Amazonka.SSM.Types.Parameter
import qualified Portfolio.Lib.Either.Main as Either
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppConfig.Main as AppConfig

data AppRemoteConfig = AppRemoteConfig
  { contentfulSpaceId :: String
  , contentfulEnvironmentId :: String
  , contentfulBaseUrl :: String
  , contentfulAccessToken :: String
  }
  deriving (Generic, Aeson.FromJSON)

fromRemoteConfig :: MonadIO m => AppConfig.AppConfig -> Except.ExceptT String m AppRemoteConfig
fromRemoteConfig config = do
  Except.ExceptT $ liftIO $ Amazonka.runResourceT do
    response <- Amazonka.send config.amazonka
      ( ( Amazonka.SSM.GetParameter.newGetParameter config.contentfulParameterName ) &
        ( Amazonka.SSM.GetParameter.getParameter_withDecryption ?~ True )
      )
    pure do
      value <- Either.fromMaybe "missing parameter"
        ( response
            ^. Amazonka.SSM.GetParameter.getParameterResponse_parameter . _Just
            .  Amazonka.SSM.Types.Parameter.parameter_value
        )
      Aeson.eitherDecode (Lib.ByteString.toLazy $ Lib.ByteString.fromUtf8Text value)
