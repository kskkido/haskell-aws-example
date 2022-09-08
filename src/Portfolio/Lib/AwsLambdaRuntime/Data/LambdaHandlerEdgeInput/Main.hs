module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEdgeInput.Main
  ( LambdaHandlerEdgeInput(..)
  ) where

import RIO
import qualified System.IO as IO
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEvent.Main as LambdaHandlerEvent
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerContext.Main as LambdaHandlerContext

data LambdaHandlerEdgeInput = LambdaHandlerEdgeInput
  { event :: LambdaHandlerEvent.LambdaHandlerEvent
  , context :: LambdaHandlerContext.LambdaHandlerContext
  }
  deriving (Generic)


