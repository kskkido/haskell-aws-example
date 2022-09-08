module Portfolio.Lib.AwsLambdaRuntime.Repositories.LambdaRuntime.Data.Repository.Main
  ( Repository(..)
  , fromAwsLambdaRuntimeApi
  ) where

import RIO hiding (exp)
import qualified System.IO as IO
import qualified Control.Retry as Retry
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Queries.Main as AwsLambdaRuntimeApi.Queries
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.Query.Main as AwsLambdaRuntimeApi.Query
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.QueryContext.Main as AwsLambdaRuntimeApi.QueryContext
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.LambdaRuntimeInvocation.Main as AwsLambdaRuntimeApi.LambdaRuntimeInvocation

data Repository = Repository
  { getNextInvocation :: IO.IO AwsLambdaRuntimeApi.LambdaRuntimeInvocation.LambdaRuntimeInvocation
  }

fromAwsLambdaRuntimeApi :: AwsLambdaRuntimeApi.QueryContext.HasQueryContext a => a -> Repository
fromAwsLambdaRuntimeApi config = Repository
  { getNextInvocation = do
      result <- Retry.recoverAll Retry.retryPolicyDefault $ const do
        AwsLambdaRuntimeApi.Query.exec config do
          AwsLambdaRuntimeApi.Queries.getNextInvocation
      either throwString pure result
  }
