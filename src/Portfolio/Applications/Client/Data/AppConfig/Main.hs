module Portfolio.Applications.Client.Data.AppConfig.Main
  ( AppConfig(..)
  ) where

import RIO
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale

data AppConfig = AppConfig
  { stage :: String
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

