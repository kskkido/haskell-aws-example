module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeEnvironment.Main
  ( LambdaRuntimeEnvironment(..)
  , HasLambdaRuntimeEnvironment(..)
  , fromSystem
  ) where

import RIO
import qualified Text.Read
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Control.Monad.Except as Except
import qualified Portfolio.Lib.Either.Main as Either
import qualified Portfolio.Lib.Parsec.Data.ParseError.Main as Lib.Parsec.ParseError
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeApiSource.Main as LambdaRuntimeApiSource

data LambdaRuntimeEnvironment = LambdaRuntimeEnvironment
  { awsLambdaLogGroupName :: String
  , awsLambdaLogStreamName :: String
  , awsLambdaFunctionVersion :: String
  , awsLambdaFunctionName :: String
  , awsLambdaFunctionMemorySize :: Int
  , xAmazonTraceId :: Maybe String
  , lambdaTaskRoot :: String
  , handlerName :: String
  , awsLambdaRuntimeApiSource :: LambdaRuntimeApiSource.LambdaRuntimeApiSource
  , awsLambdaRuntimeApiVersion :: String
  } deriving (Eq, Show)

class HasLambdaRuntimeEnvironment a where
  get :: a -> LambdaRuntimeEnvironment
instance HasLambdaRuntimeEnvironment LambdaRuntimeEnvironment where
  get = id

fromSystem :: IO.IO (Either String LambdaRuntimeEnvironment)
fromSystem = do
  Except.runExceptT do
    awsLambdaLogGroupName <- Except.ExceptT do
      Either.fromMaybe "Invalid AWS_LAMBDA_LOG_GROUP_NAME" <$> do
        Environment.lookupEnv "AWS_LAMBDA_LOG_GROUP_NAME"
    awsLambdaLogStreamName <- Except.ExceptT do
      Either.fromMaybe "Invalid AWS_LAMBDA_LOG_STREAM_NAME" <$> do
        Environment.lookupEnv "AWS_LAMBDA_LOG_STREAM_NAME"
    awsLambdaFunctionVersion <- Except.ExceptT do
      Either.fromMaybe "Invalid AWS_LAMBDA_FUNCTION_VERSION" <$> do
        Environment.lookupEnv "AWS_LAMBDA_FUNCTION_VERSION"
    awsLambdaFunctionName <- Except.ExceptT do
      Either.fromMaybe "Invalid AWS_LAMBDA_FUNCTION_NAME" <$> do
        Environment.lookupEnv "AWS_LAMBDA_FUNCTION_NAME"
    xAmazonTraceId <- Except.ExceptT do
      pure <$> do
        Environment.lookupEnv "_X_AMZN_TRACE_ID"
    lambdaTaskRoot <- Except.ExceptT do
      Either.fromMaybe "Invalid LAMBDA_TASK_ROOT" <$> do
        Environment.lookupEnv "LAMBDA_TASK_ROOT"
    handlerName <- Except.ExceptT do
      Either.fromMaybe "Invalid _HANDLER" <$> do
        Environment.lookupEnv "_HANDLER"
    awsLambdaRuntimeApi <- Except.ExceptT do
      Either.fromMaybe "Invalid AWS_LAMBDA_RUNTIME_API" <$> do
        Environment.lookupEnv "AWS_LAMBDA_RUNTIME_API"
    awsLambdaRuntimeApiSource <- Except.ExceptT do
      pure $ first Lib.Parsec.ParseError.toString $ do
        LambdaRuntimeApiSource.fromString awsLambdaRuntimeApi
    awsLambdaFunctionMemorySize <- Except.ExceptT do
      Either.fromMaybe "Invalid AWS_LAMBDA_FUNCTION_MEMORY_SIZE" <$> do
        ma <- Environment.lookupEnv "AWS_LAMBDA_FUNCTION_MEMORY_SIZE"
        pure (ma >>= Text.Read.readMaybe)
    pure $ LambdaRuntimeEnvironment
      { awsLambdaLogGroupName = awsLambdaLogGroupName
      , awsLambdaLogStreamName = awsLambdaLogStreamName
      , awsLambdaFunctionVersion = awsLambdaFunctionVersion
      , awsLambdaFunctionName = awsLambdaFunctionName
      , xAmazonTraceId = xAmazonTraceId
      , lambdaTaskRoot = lambdaTaskRoot
      , handlerName = handlerName
      , awsLambdaRuntimeApiSource = awsLambdaRuntimeApiSource
      , awsLambdaRuntimeApiVersion = "2018-06-01"
      , awsLambdaFunctionMemorySize = awsLambdaFunctionMemorySize
      }
