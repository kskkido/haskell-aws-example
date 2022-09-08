module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerApiGatewayResponse.Main
  ( LambdaHandlerApiGatewayResponse(..)
  ) where

import RIO
import qualified RIO.Text as Text
import qualified RIO.HashMap as HashMap
import qualified Data.Aeson as Aeson

data LambdaHandlerApiGatewayResponse = LambdaHandlerApiGatewayResponse
  { isBase64Encoded :: Bool
  , statusCode :: Int
  , body :: Text.Text
  , headers :: HashMap.HashMap String String
  }
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

