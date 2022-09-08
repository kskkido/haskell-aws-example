module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerApiGatewayRequest.Main
  ( LambdaHandlerApiGatewayRequest(..)
  ) where

import RIO
import qualified RIO.HashMap as HashMap
import qualified Data.Aeson as Aeson

data LambdaHandlerApiGatewayRequest a = LambdaHandlerApiGatewayRequest
  { isBase64Encoded :: Bool
  , statusCode :: Int
  , body :: a
  , headers :: HashMap.HashMap String String
  }
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)
