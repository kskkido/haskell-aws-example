module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeInput.Main
  ( LambdaRuntimeInput(..)
  , empty
  ) where

import RIO
import qualified RIO.Text as Text
import qualified RIO.HashMap as HashMap
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerMap.Main as LambdaHandlerMap

data LambdaRuntimeInput = LambdaRuntimeInput
  { handlerMap :: LambdaHandlerMap.LambdaHandlerMap
  }
  deriving (Generic)

empty :: LambdaRuntimeInput
empty = LambdaRuntimeInput
  { handlerMap = mempty
  }
