module Portfolio.Lib.AwsLambdaRuntimeApi.Data.QueryContext.Main
  ( QueryContext(..)
  , HasQueryContext(..)
  , toPath
  , toRequest
  ) where

import RIO
import qualified RIO.List as List
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP

data QueryContext = QueryContext
  { host :: String
  , port :: Int
  , version :: String
  , httpManager :: HTTP.Manager
  }
  deriving (Generic)

class HasQueryContext a where
  get :: a -> QueryContext
instance HasQueryContext QueryContext where
  get = id

toPath :: QueryContext -> String
toPath context = "/" <> List.intercalate "/"
  [ context.version
  ]

toRequest :: QueryContext -> HTTP.Request
toRequest context =
  ( ( HTTP.defaultRequest ) &
    ( HTTP.setRequestPort context.port ) &
    ( HTTP.setRequestHost $ fromString context.host )
  )
