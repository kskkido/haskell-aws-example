module Portfolio.Applications.Server.Data.AppConfig.Main
  ( AppConfig(..)
  , fromSystem
  , fromConfigDto
  ) where

import RIO
import qualified System.IO as IO
import qualified Portfolio.Lib.Parse.Main as Lib.Parse
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Applications.Server.Data.AppConfigDto.Main as AppConfigDto

data AppConfig = AppConfig
  { stage :: String
  , serverPort :: Int
  , defaultLocale :: Locale.Locale
  , filePathTranslationsEn :: String
  , filePathTranslationsJa :: String
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
  return $ dto >>= fromConfigDto

fromConfigDto :: AppConfigDto.AppConfigDto -> Either String AppConfig
fromConfigDto dto = do
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
    , contentfulSpaceId = dto.contentfulSpaceId
    , contentfulEnvironmentId = dto.contentfulEnvironmentId
    , contentfulAccessToken = dto.contentfulAccessToken
    , contentfulBaseUrl = dto.contentfulBaseUrl
    , redisHost = dto.redisHost
    , redisPort = redisPort
    , redisDatabase = redisDatabase
    }

