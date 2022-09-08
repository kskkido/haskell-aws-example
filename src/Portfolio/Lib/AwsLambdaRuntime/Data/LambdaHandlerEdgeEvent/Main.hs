module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEdgeEvent.Main
  ( LambdaHandlerEdgeEvent(..)
  , fromJSON
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as List.NonEmpty

data LambdaHandlerEdgeEvent = LambdaHandlerEdgeEvent
  { records :: List.NonEmpty.NonEmpty LambdaEdgeEventRecord
  }
  deriving (Generic, Aeson.FromJSON)

data LambdaEdgeEventRecord = LambdaEdgeEventRecord
  { cf :: LambdaEdgeEventRecordCf
  }
  deriving (Generic, Aeson.FromJSON)

data LambdaEdgeEventRecordCf = LambdaEdgeEventRecordCf
  { config :: LambdaEdgeEventRecordCfConfig
  , request :: LambdaEdgeEventRecordCfRequest
  }
  deriving (Generic, Aeson.FromJSON)

data LambdaEdgeEventRecordCfConfig = LambdaEdgeEventRecordCfConfig
  { distributionDomainName :: String
  , distributionId :: String
  , eventType :: String
  , requestId :: String
  }
  deriving (Generic, Aeson.FromJSON)

data LambdaEdgeEventRecordCfRequest = LambdaEdgeEventRecordCfRequest
  { clientIp :: String
  , headers :: LambdaEdgeEventRecordCfRequestHeaders
  , method :: String
  , querystring :: String
  , uri :: String
  }
  deriving (Generic, Aeson.FromJSON)

data LambdaEdgeEventRecordCfRequestHeaders = LambdaEdgeEventRecordCfRequestHeaders
  { host :: LambdaEdgeEventRecordCfRequestHeader
  , userAgent :: LambdaEdgeEventRecordCfRequestHeader
  , accept :: LambdaEdgeEventRecordCfRequestHeader
  }
  deriving (Generic)

instance Aeson.FromJSON LambdaEdgeEventRecordCfRequestHeaders where
  parseJSON = Aeson.withObject "lambdaEdgeEventRecordCfRequestHeaders" $ \x -> LambdaEdgeEventRecordCfRequestHeaders
    <$> x Aeson..: "host"
    <*> x Aeson..: "user-agent"
    <*> x Aeson..: "accept"

data LambdaEdgeEventRecordCfRequestHeader = LambdaEdgeEventRecordCfRequestHeader
  { key :: String
  , value :: String
  }
  deriving (Generic, Aeson.FromJSON)

fromJSON :: Aeson.Value -> Aeson.Result LambdaHandlerEdgeEvent
fromJSON = Aeson.fromJSON
