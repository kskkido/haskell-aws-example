module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerOutput.Main
  ( LambdaHandlerOutput
  ) where

import RIO
import qualified Network.HTTP.Client.Conduit as Http

type LambdaHandlerOutput = Http.RequestBody
