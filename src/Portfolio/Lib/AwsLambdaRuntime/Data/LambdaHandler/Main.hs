module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandler.Main
  ( LambdaHandler
  , LambdaHandlerInvocation
  , fromInvocation
  , fromInvocationFn
  ) where

import RIO
import qualified System.IO as IO
import qualified Data.Aeson as Aeson
import qualified Control.Monad.Reader as Reader
import qualified Network.HTTP.Client.Conduit as Http
import qualified Portfolio.Lib.Monad.Main as Monad
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerContext.Main as LambdaHandlerContext
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerInput.Main as LambdaHandlerInput
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerOutput.Main as LambdaHandlerOutput

type LambdaHandler = LambdaHandlerInput.LambdaHandlerInput -> IO.IO LambdaHandlerOutput.LambdaHandlerOutput

type LambdaHandlerInvocation a = Reader.ReaderT LambdaHandlerContext.LambdaHandlerContext IO.IO a

fromInvocation :: (Aeson.ToJSON b) => LambdaHandlerInvocation b -> LambdaHandler
fromInvocation fn input = do
  output <- Reader.runReaderT fn input.context
  pure $ Http.RequestBodyLBS $ Aeson.encode output

fromInvocationFn :: (Aeson.FromJSON a, Aeson.ToJSON b) => (a -> LambdaHandlerInvocation b) -> LambdaHandler
fromInvocationFn fn input = do
  event <- Monad.mfold $ Aeson.fromJSON input.event
  output <- Reader.runReaderT (fn event) input.context
  pure $ Http.RequestBodyLBS $ Aeson.encode output

