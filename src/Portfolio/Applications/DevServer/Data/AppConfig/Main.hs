module Portfolio.Applications.DevServer.Data.AppConfig.Main
  ( AppConfig(..)
  , fromSystem
  , fromConfigDto
  , toStaticSiteGeneratorAppConfig
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified Portfolio.Lib.Parse.Main as Lib.Parse
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppConfig.Main as StaticSiteGenerator.AppConfig
import qualified Portfolio.Applications.DevServer.Data.AppConfigDto.Main as AppConfigDto

data AppConfig = AppConfig
  { stage :: String
  , serverPort :: Int
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

fromConfigDto :: AppConfigDto.AppConfigDto -> IO.IO (Either String AppConfig)
fromConfigDto dto = do
  dirname <- Directory.getCurrentDirectory
  pure $ do
    defaultLocale <- maybe (Left "unable to parse default locale") Right $ Locale.fromString dto.defaultLocale
    serverPort <- maybe (Left "unable to parse server port") Right $ Lib.Parse.parseInt dto.serverPort
    redisPort <- maybe (Left "unable to parse redis port") Right $ Lib.Parse.parseInt dto.redisPort
    redisDatabase <- maybe (Left "unable to parse redis database") Right $ Lib.Parse.parseInt dto.redisDatabase
    return AppConfig
      { stage = dto.stage
      , serverPort = serverPort
      , defaultLocale = defaultLocale
      , filePathTranslationsEn = dto.filePathTranslationsEn
      , filePathTranslationsJa = dto.filePathTranslationsJa
      , filePathBuild = dirname <> "/" <> dto.filePathBuild
      , filePathStatic = dirname <> "/" <> dto.filePathStatic
      , contentfulSpaceId = dto.contentfulSpaceId
      , contentfulEnvironmentId = dto.contentfulEnvironmentId
      , contentfulAccessToken = dto.contentfulAccessToken
      , contentfulBaseUrl = dto.contentfulBaseUrl
      , redisHost = dto.redisHost
      , redisPort = redisPort
      , redisDatabase = redisDatabase
      }

toStaticSiteGeneratorAppConfig :: AppConfig -> StaticSiteGenerator.AppConfig.AppConfig
toStaticSiteGeneratorAppConfig config =
  StaticSiteGenerator.AppConfig.AppConfig
    { StaticSiteGenerator.AppConfig.stage = config.stage
    , StaticSiteGenerator.AppConfig.defaultLocale = config.defaultLocale
    , StaticSiteGenerator.AppConfig.filePathTranslationsEn = config.filePathTranslationsEn
    , StaticSiteGenerator.AppConfig.filePathTranslationsJa = config.filePathTranslationsJa
    , StaticSiteGenerator.AppConfig.filePathBuild = config.filePathBuild
    , StaticSiteGenerator.AppConfig.filePathStatic = config.filePathStatic
    , StaticSiteGenerator.AppConfig.contentfulSpaceId = config.contentfulSpaceId
    , StaticSiteGenerator.AppConfig.contentfulEnvironmentId = config.contentfulEnvironmentId
    , StaticSiteGenerator.AppConfig.contentfulAccessToken = config.contentfulAccessToken
    , StaticSiteGenerator.AppConfig.contentfulBaseUrl = config.contentfulBaseUrl
    , StaticSiteGenerator.AppConfig.redisHost = config.redisHost
    , StaticSiteGenerator.AppConfig.redisPort = config.redisPort
    , StaticSiteGenerator.AppConfig.redisDatabase = config.redisDatabase
    }
