module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppConfig.Main
  ( AppConfig(..)
  , fromSystem
  ) where

import RIO
import qualified RIO.Text as Text
import qualified System.Environment as Environment
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Except as Except
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified Amazonka
import qualified Amazonka.Env
import qualified Portfolio.Lib.Amazonka.Auth.Main as Lib.Amazonka.Auth

data AppConfig = AppConfig
  { amazonka :: Amazonka.Env.Env
  , pushBucketName :: Text.Text
  , distributionId :: Text.Text
  , contentfulParameterName :: Text.Text
  , lambdaTaskRoot :: String
  , stage :: String
  , defaultLocale :: String
  , filePathTranslationsEn :: String
  , filePathTranslationsJa :: String
  , filePathBuild :: String
  , filePathStatic :: String
  , contentfulBaseUrl :: String
  , contentfulSpaceId :: String
  , contentfulEnvironmentId :: String
  , contentfulAccessToken :: String
  , redisHost :: String
  , redisPort :: String
  , redisDatabase :: String
  }
  deriving (Generic)

fromSystem :: MonadIO m => Except.ExceptT String m AppConfig
fromSystem = Except.ExceptT $ liftIO do
  httpManager <- Client.newManager Client.Conduit.tlsManagerSettings
  config <- Maybe.runMaybeT do
    amazonka <- liftIO do
      auth <- second (Maybe.fromMaybe Amazonka.Tokyo) <$> Lib.Amazonka.Auth.fromSystem httpManager
      pure $ Lib.Amazonka.Auth.authenticate auth (Amazonka.newEnvWith httpManager)
    pushBucketName <- do
      value <- Maybe.MaybeT $ Environment.lookupEnv "PUSH_BUCKET_NAME"
      pure $ Text.pack value
    distributionId <- do
      value <- Maybe.MaybeT $ Environment.lookupEnv "DISTRIBUTION_ID"
      pure $ Text.pack value
    contentfulParameterName <- do
      value <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_PARAMETER_NAME"
      pure $ Text.pack value
    lambdaTaskRoot <- Maybe.MaybeT $ Environment.lookupEnv "LAMBDA_TASK_ROOT"
    stage <- Maybe.MaybeT $ Environment.lookupEnv "STAGE"
    defaultLocale <- Maybe.MaybeT $ Environment.lookupEnv "DEFAULT_LOCALE"
    filePathTranslationsEn <- Maybe.MaybeT $ Environment.lookupEnv "FILE_PATH_TRANSLATIONS_EN"
    filePathTranslationsJa <- Maybe.MaybeT $ Environment.lookupEnv "FILE_PATH_TRANSLATIONS_JA"
    filePathBuild <- Maybe.MaybeT $ Environment.lookupEnv "FILE_PATH_BUILD"
    filePathStatic <- Maybe.MaybeT $ Environment.lookupEnv "FILE_PATH_STATIC"
    contentfulBaseUrl <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_BASE_URL"
    contentfulSpaceId <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_SPACE_ID"
    contentfulEnvironmentId <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_ENVIRONMENT_ID"
    contentfulAccessToken <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_ACCESS_TOKEN"
    redisHost <- Maybe.MaybeT $ Environment.lookupEnv "REDIS_HOST"
    redisPort <- Maybe.MaybeT $ Environment.lookupEnv "REDIS_PORT"
    redisDatabase <- Maybe.MaybeT $ Environment.lookupEnv "REDIS_DATABASE"
    pure $ AppConfig
      { amazonka = amazonka
      , pushBucketName = pushBucketName
      , distributionId = distributionId
      , contentfulParameterName = contentfulParameterName
      , lambdaTaskRoot = lambdaTaskRoot
      , stage = stage
      , defaultLocale = defaultLocale
      , filePathTranslationsEn = filePathTranslationsEn
      , filePathTranslationsJa = filePathTranslationsJa
      , filePathBuild = filePathBuild
      , filePathStatic = filePathStatic
      , contentfulBaseUrl = contentfulBaseUrl
      , contentfulSpaceId = contentfulSpaceId
      , contentfulEnvironmentId = contentfulEnvironmentId
      , contentfulAccessToken = contentfulAccessToken
      , redisHost = redisHost
      , redisPort = redisPort
      , redisDatabase = redisDatabase
      }
  pure $ maybe (Left "[GenerateStaticSite] Unable to parse System Env") Right config
