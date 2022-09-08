module Portfolio.Lib.Contentful.Data.Config.Main
  ( Config(..)
  , HasConfig(..)
  , toRequestHeaders
  , toQuery
  , toPath
  ) where

import RIO
import qualified RIO.List as List
import qualified Data.Maybe as Maybe
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.ByteString.UTF8 as ByteString.UTF8
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.URI as URI

data Config = Config
  { spaceId :: String
  , accessToken :: String
  , environmentId :: String
  , baseUrl :: String
  , locale :: Maybe.Maybe String
  }

class HasConfig a where
  get :: a -> Config
instance HasConfig Config where
  get = id

toRequestHeaders :: Config -> HTTP.RequestHeaders
toRequestHeaders config =
  [ ( CaseInsensitive.mk $ ByteString.UTF8.fromString "Authorization",
      ByteString.UTF8.fromString $ "Bearer " <> config.accessToken
    )
  ]

toQuery :: Config -> URI.Query
toQuery config =
  [ (ByteString.UTF8.fromString "access_token", pure $ ByteString.UTF8.fromString config.accessToken)
  , (ByteString.UTF8.fromString "locale", ByteString.UTF8.fromString <$> config.locale)
  ]

toPath :: Config -> String
toPath config = "/" <> List.intercalate "/"
  [ "spaces"
  , config.spaceId
  , "environments"
  , config.environmentId
  ]
