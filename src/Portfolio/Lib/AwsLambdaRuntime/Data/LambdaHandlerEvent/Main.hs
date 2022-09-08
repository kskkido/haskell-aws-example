module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEvent.Main
  ( LambdaHandlerEvent
  ) where

import qualified Data.Aeson as Aeson

type LambdaHandlerEvent = Aeson.Value
