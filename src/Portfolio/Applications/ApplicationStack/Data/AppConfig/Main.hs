module Portfolio.Applications.ApplicationStack.Data.AppConfig.Main
  ( AppConfig(..)
  , fromSystem
  , toLambdaEnv
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Conduit as Client.Conduit
import qualified Amazonka
import qualified Amazonka.Env
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Control.Monad.Except as Except
import qualified Portfolio.Lib.Either.Main as Either
import qualified Portfolio.Lib.Amazonka.Auth.Main as Lib.Amazonka.Auth

data AppConfig = AppConfig
  { stage :: String
  , stackName :: String
  , serviceName :: String
  , lambdaVersion :: String
  , lambdaPullFilePath :: String
  , lambdaPushFilePath :: String
  , lambdaBucketName :: String
  , staticSiteBucketNamePrefix :: String
  , defaultLocale :: String
  , filePathTranslationsEn :: String
  , filePathTranslationsJa :: String
  , filePathBuild :: String
  , filePathStatic :: String
  , contentfulParameterName :: String
  , contentfulSpaceId :: String
  , contentfulAccessToken :: String
  , contentfulEnvironmentId :: String
  , contentfulBaseUrl :: String
  , redisHost :: String
  , redisPort :: String
  , redisDatabase :: String
  , amazonka :: Amazonka.Env.Env
  } deriving (Generic)

fromSystem :: IO.IO (Either String AppConfig)
fromSystem = do
  httpManager <- Client.newManager Client.Conduit.tlsManagerSettings
  Except.runExceptT do
    stage <- Except.ExceptT do
      Either.fromMaybe "Invalid STAGE" <$> do
        Environment.lookupEnv "STAGE"
    stackName <- Except.ExceptT do
      Either.fromMaybe "Invalid STACK_NAME" <$> do
        Environment.lookupEnv "STACK_NAME"
    serviceName <- Except.ExceptT do
      Either.fromMaybe "Invalid SERVICE_NAME" <$> do
        Environment.lookupEnv "SERVICE_NAME"
    staticSiteBucketNamePrefix <- Except.ExceptT do
      Either.fromMaybe "Invalid STATIC_SITE_BUCKET_NAME_PREFIX" <$> do
        Environment.lookupEnv "STATIC_SITE_BUCKET_NAME_PREFIX"
    lambdaVersion <- Except.ExceptT do
      Either.fromMaybe "Invalid LAMBDA_VERSION" <$> do
        Environment.lookupEnv "LAMBDA_VERSION"
    lambdaFilePath <- Except.ExceptT do
      Either.fromMaybe "Invalid LAMBDA_FILE_PATH" <$> do
        Environment.lookupEnv "LAMBDA_FILE_PATH"
    lambdaBucketName <- Except.ExceptT do
      Either.fromMaybe "Invalid LAMBDA_BUCKET_NAME" <$> do
        Environment.lookupEnv "LAMBDA_BUCKET_NAME"
    defaultLocale <- Except.ExceptT do
      Either.fromMaybe "Invalid DEFAULT_LOCALE" <$> do
        Environment.lookupEnv "DEFAULT_LOCALE"
    filePathTranslationsEn <- Except.ExceptT do
      Either.fromMaybe "Invalid FILE_PATH_TRANSLATIONS_EN" <$> do
        Environment.lookupEnv "FILE_PATH_TRANSLATIONS_EN"
    filePathTranslationsJa <- Except.ExceptT do
      Either.fromMaybe "Invalid FILE_PATH_TRANSLATIONS_JA" <$> do
        Environment.lookupEnv "FILE_PATH_TRANSLATIONS_JA"
    filePathBuild <- Except.ExceptT do
      Either.fromMaybe "Invalid FILE_PATH_BUILD" <$> do
        Environment.lookupEnv "FILE_PATH_BUILD"
    filePathStatic <- Except.ExceptT do
      Either.fromMaybe "Invalid FILE_PATH_STATIC" <$> do
        Environment.lookupEnv "FILE_PATH_STATIC"
    contentfulParameterName <- Except.ExceptT do
      Either.fromMaybe "Invalid CONTENTFUL_PARAMETER_NAME" <$> do
        Environment.lookupEnv "CONTENTFUL_PARAMETER_NAME"
    contentfulSpaceId <- Except.ExceptT do
      Either.fromMaybe "Invalid CONTENTFUL_SPACE_ID" <$> do
        Environment.lookupEnv "CONTENTFUL_SPACE_ID"
    contentfulEnvironmentId <- Except.ExceptT do
      Either.fromMaybe "Invalid CONTENTFUL_ENVIRONMENT_ID" <$> do
        Environment.lookupEnv "CONTENTFUL_ENVIRONMENT_ID"
    contentfulAccessToken <- Except.ExceptT do
      Either.fromMaybe "Invalid CONTENTFUL_ACCESS_TOKEN" <$> do
        Environment.lookupEnv "CONTENTFUL_ACCESS_TOKEN"
    contentfulBaseUrl <- Except.ExceptT do
      Either.fromMaybe "Invalid CONTENTFUL_BASE_URL" <$> do
        Environment.lookupEnv "CONTENTFUL_BASE_URL"
    redisHost <- Except.ExceptT do
      Either.fromMaybe "Invalid REDIS_HOST" <$> do
        Environment.lookupEnv "REDIS_HOST"
    redisPort <- Except.ExceptT do
      Either.fromMaybe "Invalid REDIS_PORT" <$> do
        Environment.lookupEnv "REDIS_PORT"
    redisDatabase <- Except.ExceptT do
      Either.fromMaybe "Invalid REDIS_DATABASE" <$> do
        Environment.lookupEnv "REDIS_DATABASE"
    amazonka <- liftIO do
      auth <- second (Maybe.fromMaybe Amazonka.Tokyo) <$> Lib.Amazonka.Auth.fromSystem httpManager
      pure $ Lib.Amazonka.Auth.authenticate auth (Amazonka.newEnvWith httpManager)
    return $ AppConfig
      { stage = stage
      , stackName = stackName
      , serviceName = serviceName
      , staticSiteBucketNamePrefix = staticSiteBucketNamePrefix
      , lambdaVersion = lambdaVersion
      , lambdaPullFilePath = lambdaFilePath
      , lambdaPushFilePath = lambdaVersion <> "/" <> lambdaFilePath
      , lambdaBucketName = lambdaBucketName
      , defaultLocale = defaultLocale
      , filePathTranslationsEn = filePathTranslationsEn
      , filePathTranslationsJa = filePathTranslationsJa
      , filePathBuild = filePathBuild
      , filePathStatic = filePathStatic
      , contentfulParameterName = contentfulParameterName
      , contentfulSpaceId = contentfulSpaceId
      , contentfulEnvironmentId = contentfulEnvironmentId
      , contentfulAccessToken = contentfulAccessToken
      , contentfulBaseUrl = contentfulBaseUrl
      , redisHost = redisHost
      , redisPort = redisPort
      , redisDatabase = redisDatabase
      , amazonka = amazonka
      }

toLambdaEnv :: AppConfig -> Aeson.Object
toLambdaEnv config = Aeson.KeyMap.fromList $
  second Aeson.toJSON <$>
  [ ("STAGE", config.stage)
  , ("STACK_NAME", config.stackName)
  , ("SERVICE_NAME", config.serviceName)
  , ("DEFAULT_LOCALE", config.defaultLocale)
  , ("VERSION", config.lambdaVersion)
  , ("FILE_PATH_TRANSLATIONS_EN", config.filePathTranslationsEn)
  , ("FILE_PATH_TRANSLATIONS_JA", config.filePathTranslationsJa)
  , ("FILE_PATH_BUILD", config.filePathBuild)
  , ("FILE_PATH_STATIC", config.filePathStatic)
  , ("CONTENTFUL_PARAMETER_NAME", config.contentfulParameterName)
  , ("CONTENTFUL_SPACE_ID", config.contentfulSpaceId)
  , ("CONTENTFUL_ENVIRONMENT_ID", config.contentfulEnvironmentId)
  , ("CONTENTFUL_ACCESS_TOKEN", config.contentfulAccessToken)
  , ("CONTENTFUL_BASE_URL", config.contentfulBaseUrl)
  , ("REDIS_HOST", config.redisHost)
  , ("REDIS_PORT", config.redisPort)
  , ("REDIS_DATABASE", config.redisDatabase)
  ]
