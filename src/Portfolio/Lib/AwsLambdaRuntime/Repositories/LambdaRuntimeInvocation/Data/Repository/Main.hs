module Portfolio.Lib.AwsLambdaRuntime.Repositories.LambdaRuntimeInvocation.Data.Repository.Main
  ( Repository(..)
  , fromAwsLambdaRuntimeApi
  ) where

import RIO hiding (exp)
import qualified System.IO as IO
import qualified Control.Retry as Retry
import qualified Network.HTTP.Client.Conduit as Http
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Queries.Main as AwsLambdaRuntimeApi.Queries
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.Query.Main as AwsLambdaRuntimeApi.Query
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.QueryContext.Main as AwsLambdaRuntimeApi.QueryContext
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.PostLambdaRuntimeInvocationResponseParameters.Main as PostLambdaRuntimeInvocationResponseParameters
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.PostLambdaRuntimeInvocationErrorParameters.Main as PostLambdaRuntimeInvocationErrorParameters

data Repository = Repository
  { postResponse :: Http.RequestBody -> IO.IO ()
  , postError :: Http.RequestBody -> IO.IO ()
  }

fromAwsLambdaRuntimeApi :: AwsLambdaRuntimeApi.QueryContext.HasQueryContext a => a -> String -> Repository
fromAwsLambdaRuntimeApi config requestId = Repository
  { postResponse = \body -> do
      result <- Retry.retrying Retry.retryPolicyDefault (const $ return . isLeft) $ \_ -> do
        AwsLambdaRuntimeApi.Query.exec config do
          AwsLambdaRuntimeApi.Queries.postInvocationResponse $ PostLambdaRuntimeInvocationResponseParameters.PostLambdaRuntimeInvocationResponseParameters
            { PostLambdaRuntimeInvocationResponseParameters.requestId = requestId
            , PostLambdaRuntimeInvocationResponseParameters.body = body
            }
      either throwString pure result
  , postError = \body -> do
      result <- Retry.retrying Retry.retryPolicyDefault (const $ return . isLeft) $ \_ -> do
        AwsLambdaRuntimeApi.Query.exec config do
          AwsLambdaRuntimeApi.Queries.postInvocationError $ PostLambdaRuntimeInvocationErrorParameters.PostLambdaRuntimeInvocationErrorParameters
            { PostLambdaRuntimeInvocationErrorParameters.requestId = requestId
            , PostLambdaRuntimeInvocationErrorParameters.body = body
            }
      either throwString pure result
  }
