module Portfolio.Applications.Server.Data.AppConfigDto.Main
  ( AppConfigDto(..)
  , fromSystem
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Control.Monad.Trans.Maybe as Maybe

data AppConfigDto = AppConfigDto
  { stage :: String
  , serverPort :: String
  , defaultLocale :: String
  , filePathTranslationsEn :: String
  , filePathTranslationsJa :: String
  , contentfulSpaceId :: String
  , contentfulAccessToken :: String
  , contentfulEnvironmentId :: String
  , contentfulBaseUrl :: String
  , redisHost :: String
  , redisPort :: String
  , redisDatabase :: String
  } deriving (Eq, Show)

fromSystem :: IO.IO (Either String AppConfigDto)
fromSystem = do
  env <- Maybe.runMaybeT do
    stage <- Maybe.MaybeT $ Environment.lookupEnv "STAGE"
    serverPort <- Maybe.MaybeT $ Environment.lookupEnv "SERVER_PORT"
    defaultLocale <- Maybe.MaybeT $ Environment.lookupEnv "DEFAULT_LOCALE"
    filePathTranslationsEn <- Maybe.MaybeT $ Environment.lookupEnv "FILE_PATH_TRANSLATIONS_EN"
    filePathTranslationsJa <- Maybe.MaybeT $ Environment.lookupEnv "FILE_PATH_TRANSLATIONS_JA"
    contentfulSpaceId <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_SPACE_ID"
    contentfulEnvironmentId <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_ENVIRONMENT_ID"
    contentfulAccessToken <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_ACCESS_TOKEN"
    contentfulBaseUrl <- Maybe.MaybeT $ Environment.lookupEnv "CONTENTFUL_BASE_URL"
    redisHost <- Maybe.MaybeT $ Environment.lookupEnv "REDIS_HOST"
    redisPort <- Maybe.MaybeT $ Environment.lookupEnv "REDIS_PORT"
    redisDatabase <- Maybe.MaybeT $ Environment.lookupEnv "REDIS_DATABASE"
    return $ AppConfigDto
      { stage = stage
      , serverPort = serverPort
      , defaultLocale = defaultLocale
      , filePathTranslationsEn = filePathTranslationsEn
      , filePathTranslationsJa = filePathTranslationsJa
      , contentfulSpaceId = contentfulSpaceId
      , contentfulEnvironmentId = contentfulEnvironmentId
      , contentfulAccessToken = contentfulAccessToken
      , contentfulBaseUrl = contentfulBaseUrl
      , redisHost = redisHost
      , redisPort = redisPort
      , redisDatabase = redisDatabase
      }
  return $ maybe (Left "Unable to parse System Env") Right env

