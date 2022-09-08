module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerMap.Main
  ( LambdaHandlerMap
  ) where

import RIO
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandler.Main as LambdaHandler

type LambdaHandlerMap = HashMap String LambdaHandler.LambdaHandler

