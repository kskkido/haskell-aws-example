module Portfolio.Lib.AwsLambdaRuntimeApi.Data.LambdaRuntimeInvocation.Main
  ( LambdaRuntimeInvocation(..)
  , fromResponse
  , empty
  ) where

import RIO hiding (traceId)
import qualified Text.Read
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Control.Monad as Monad
import qualified Network.HTTP.Simple as HTTP
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Lib.Function.Main as Lib.Function

data LambdaRuntimeInvocation = LambdaRuntimeInvocation
  { deadlineMs :: Int
  , traceId :: String
  , awsRequestId :: String
  , invokedFunctionArn :: String
  , handlerEvent :: Aeson.Value
  }

fromResponse :: HTTP.Response ByteString.ByteString -> Either String LambdaRuntimeInvocation
fromResponse response = do
  let headers = HTTP.getResponseHeaders response
      body = HTTP.getResponseBody response
  value <- Aeson.eitherDecode $ Lib.ByteString.toLazy body
  Lib.Function.shiftA3 Monad.foldM (empty { handlerEvent = value }) headers $ \event (header, bs) -> do
    let cs = Lib.ByteString.toString bs
    case header of
      "Lambda-Runtime-Deadline-Ms" ->
        case Text.Read.readMaybe cs of
          Just ms -> pure event { deadlineMs = ms }
          Nothing -> Left "Could not parse deadlineMs"
      "Lambda-Runtime-Trace-Id" ->
        pure event { traceId = cs }
      "Lambda-Runtime-Aws-Request-Id" ->
        pure event { awsRequestId = cs }
      "Lambda-Runtime-Invoked-Function-Arn" ->
        pure event { invokedFunctionArn = cs }
      _ ->
        pure event

empty :: LambdaRuntimeInvocation
empty = LambdaRuntimeInvocation
  { deadlineMs = 0
  , traceId = ""
  , awsRequestId = ""
  , invokedFunctionArn = ""
  , handlerEvent = ""
  }

