module Portfolio.Lib.AwsLambdaRuntimeApi.Data.PostLambdaRuntimeInvocationErrorParameters.Main
  ( PostLambdaRuntimeInvocationErrorParameters(..)
  ) where

import RIO
import qualified Network.HTTP.Client.Conduit as Http

data PostLambdaRuntimeInvocationErrorParameters = PostLambdaRuntimeInvocationErrorParameters
  { requestId :: String
  , body :: Http.RequestBody
  } deriving (Generic)


