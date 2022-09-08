module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerContext.Main
  ( LambdaHandlerContext(..)
  , fromRuntimeInvocation
  ) where

import RIO
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.App.Main as App
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.LambdaRuntimeInvocation.Main as LambdaRuntimeInvocation
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeEnvironment.Main as LambdaRuntimeEnvironment

data LambdaHandlerContext = LambdaHandlerContext
  { memoryLimitInMb :: Int,
    functionName :: String,
    functionVersion :: String,
    invokedFunctionArn :: String,
    awsRequestId :: String,
    xRayTraceId :: String,
    logStreamName :: String,
    logGroupName :: String,
    deadline :: Int
  }
  deriving (Generic)

fromRuntimeInvocation :: (LambdaRuntimeEnvironment.HasLambdaRuntimeEnvironment r, Monad m) => LambdaRuntimeInvocation.LambdaRuntimeInvocation -> App.App r m LambdaHandlerContext
fromRuntimeInvocation invocation = do
  environment <- Reader.asks LambdaRuntimeEnvironment.get
  pure $ LambdaHandlerContext
    { memoryLimitInMb = environment.awsLambdaFunctionMemorySize
    , functionName = environment.awsLambdaFunctionName
    , functionVersion = environment.awsLambdaFunctionVersion
    , invokedFunctionArn = invocation.invokedFunctionArn
    , awsRequestId = invocation.awsRequestId
    , xRayTraceId = invocation.traceId
    , logStreamName = environment.awsLambdaLogStreamName
    , logGroupName = environment.awsLambdaLogGroupName
    , deadline = invocation.deadlineMs
    }
