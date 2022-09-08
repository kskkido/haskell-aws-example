module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppContext.Main
  ( AppContext(..)
  , fromConfig
  ) where

import RIO
import qualified RIO.Text as Text
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified Amazonka
import qualified Amazonka.Env
import qualified Portfolio.Lib.Maybe.Main as Maybe
import qualified Portfolio.Lib.Amazonka.Auth.Main as Lib.Amazonka.Auth
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppConfig.Main as StaticSiteGenerator.AppConfig
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppConfigDto.Main as StaticSiteGenerator.AppConfigDto
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppRemoteConfig.Main as AppRemoteConfig

data AppContext = AppContext
  { amazonka :: Amazonka.Env.Env
  , pushBucketName :: Text.Text
  , distributionId :: Text.Text
  , staticSiteGenerator :: StaticSiteGenerator.AppConfig.AppConfig
  }
  deriving (Generic)

fromConfig :: MonadIO m => AppConfig.AppConfig -> Except.ExceptT String m AppContext
fromConfig config = do
  staticSiteGenerator <- Except.ExceptT $ liftIO $ do
    let configDto = StaticSiteGenerator.AppConfigDto.AppConfigDto
          { stage = config.stage
          , defaultLocale = config.defaultLocale
          , filePathTranslationsEn = config.filePathTranslationsEn
          , filePathTranslationsJa = config.filePathTranslationsJa
          , filePathBuild = config.filePathBuild
          , filePathStatic = config.filePathStatic
          , contentfulSpaceId = config.contentfulSpaceId
          , contentfulEnvironmentId = config.contentfulEnvironmentId
          , contentfulAccessToken = config.contentfulAccessToken
          , contentfulBaseUrl = config.contentfulBaseUrl
          , redisHost = config.redisHost
          , redisPort = config.redisPort
          , redisDatabase = config.redisDatabase
          }
    StaticSiteGenerator.AppConfig.fromConfigDto configDto
  pure $ AppContext
    { amazonka = config.amazonka
    , pushBucketName = config.pushBucketName
    , distributionId = config.distributionId
    , staticSiteGenerator = staticSiteGenerator
    }
