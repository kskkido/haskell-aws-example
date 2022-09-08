module Portfolio.Applications.StaticSiteGenerator.Data.AppConfig.Main
  ( AppConfig(..)
  , fromSystem
  , fromConfigDto
  , toClientAppConfig
  ) where

import RIO
import qualified System.IO as IO
import qualified Portfolio.Lib.Parse.Main as Lib.Parse
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Applications.Client.Data.AppConfig.Main as Client.AppConfig
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppConfigDto.Main as AppConfigDto

data AppConfig = AppConfig
  { stage :: String
  , defaultLocale :: Locale.Locale
  , filePathTranslationsEn :: String
  , filePathTranslationsJa :: String
  , filePathBuild :: String
  , filePathStatic :: String
  , contentfulSpaceId :: String
  , contentfulAccessToken :: String
  , contentfulEnvironmentId :: String
  , contentfulBaseUrl :: String
  , redisHost :: String
  , redisPort :: Int
  , redisDatabase :: Int
  } deriving (Eq, Show)

fromSystem :: IO.IO (Either String AppConfig)
fromSystem = do
  dto <- AppConfigDto.fromSystem
  either (pure . Left) fromConfigDto dto

fromConfigDto :: AppConfigDto.AppConfigDto -> IO (Either String AppConfig)
fromConfigDto dto = do
  pure $ do
    defaultLocale <- maybe (Left "unable to parse default locale") Right $ Locale.fromString dto.defaultLocale
    redisPort <- maybe (Left "unable to parse redis port") Right $ Lib.Parse.parseInt dto.redisPort
    redisDatabase <- maybe (Left "unable to parse redis database") Right $ Lib.Parse.parseInt dto.redisDatabase
    return AppConfig
      { stage = dto.stage
      , defaultLocale = defaultLocale
      , filePathTranslationsEn = dto.filePathTranslationsEn
      , filePathTranslationsJa = dto.filePathTranslationsJa
      , filePathBuild = dto.filePathBuild
      , filePathStatic = dto.filePathStatic
      , contentfulSpaceId = dto.contentfulSpaceId
      , contentfulEnvironmentId = dto.contentfulEnvironmentId
      , contentfulAccessToken = dto.contentfulAccessToken
      , contentfulBaseUrl = dto.contentfulBaseUrl
      , redisHost = dto.redisHost
      , redisPort = redisPort
      , redisDatabase = redisDatabase
      }

toClientAppConfig :: AppConfig -> Client.AppConfig.AppConfig
toClientAppConfig config = Client.AppConfig.AppConfig
    { Client.AppConfig.stage = config.stage
    , Client.AppConfig.defaultLocale = config.defaultLocale
    , Client.AppConfig.filePathTranslationsEn = config.filePathTranslationsEn
    , Client.AppConfig.filePathTranslationsJa = config.filePathTranslationsJa
    , Client.AppConfig.contentfulSpaceId = config.contentfulSpaceId
    , Client.AppConfig.contentfulEnvironmentId = config.contentfulEnvironmentId
    , Client.AppConfig.contentfulAccessToken = config.contentfulAccessToken
    , Client.AppConfig.contentfulBaseUrl = config.contentfulBaseUrl
    , Client.AppConfig.redisHost = config.redisHost
    , Client.AppConfig.redisPort = config.redisPort
    , Client.AppConfig.redisDatabase = config.redisDatabase
    }
