module Portfolio.Lib.AwsLambdaRuntimeApi.Data.PostLambdaRuntimeInvocationResponseParameters.Main
  ( PostLambdaRuntimeInvocationResponseParameters(..)
  ) where

import RIO
import qualified Network.HTTP.Client.Conduit as Http

data PostLambdaRuntimeInvocationResponseParameters = PostLambdaRuntimeInvocationResponseParameters
  { requestId :: String
  , body :: Http.RequestBody
  } deriving (Generic)

