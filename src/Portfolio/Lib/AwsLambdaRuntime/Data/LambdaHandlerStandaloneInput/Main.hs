module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerStandaloneInput.Main
  ( LambdaHandlerStandaloneInput
  ) where

import RIO
import qualified System.IO as IO
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEvent.Main as LambdaHandlerEvent
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerContext.Main as LambdaHandlerContext

data LambdaHandlerStandaloneInput = LambdaHandlerStandaloneInput
  { event :: LambdaHandlerEvent.LambdaHandlerEvent
  , context :: LambdaHandlerContext.LambdaHandlerContext
  }
  deriving (Generic)

