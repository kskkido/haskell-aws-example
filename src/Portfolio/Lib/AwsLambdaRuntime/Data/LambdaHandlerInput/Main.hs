module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerInput.Main
  ( LambdaHandlerInput(..)
  ) where

import RIO
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEvent.Main as LambdaHandlerEvent
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerContext.Main as LambdaHandlerContext

data LambdaHandlerInput = LambdaHandlerInput
  { event :: LambdaHandlerEvent.LambdaHandlerEvent
  , context :: LambdaHandlerContext.LambdaHandlerContext
  }
  deriving (Generic)

