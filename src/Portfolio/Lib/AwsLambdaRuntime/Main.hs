module Portfolio.Lib.AwsLambdaRuntime.Main
  ( run
  ) where

import RIO
import qualified System.IO as IO
import qualified System.Environment as Environment
import qualified Network.HTTP.Client.Conduit as Http
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Reader as Reader
import qualified Portfolio.Lib.Reader.Main as Reader
import qualified Portfolio.Lib.ByteString.Main as Lib.ByteString
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.App.Main as App
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeInput.Main as LambdaRuntimeInput
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeContext.Main as LambdaRuntimeContext
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeInvocationContext.Main as LambdaRuntimeInvocationContext

run :: MonadIO m => LambdaRuntimeInput.LambdaRuntimeInput -> m ()
run input = do
  runtimeContext <- LambdaRuntimeContext.fromInput input
  forever do
    App.exec runtimeContext do
      Reader.localM (lift . LambdaRuntimeContext.refresh) do
        runtimeInvocationContext <- LambdaRuntimeInvocationContext.fromRuntime
        App.exec runtimeInvocationContext do
          runtimeInvocationRepository <- Reader.asks LambdaRuntimeInvocationContext.runtimeInvocationRepository
          handler <- Reader.asks LambdaRuntimeInvocationContext.handler
          handlerInput <- Reader.asks LambdaRuntimeInvocationContext.toLambdaHandlerInput
          liftIO do
            Environment.setEnv "_X_AMZN_TRACE_ID" handlerInput.context.xRayTraceId
            handlerOutput <- handler handlerInput
            IO.hFlush IO.stdout
            IO.hFlush IO.stderr
            runtimeInvocationRepository.postResponse handlerOutput
              `Catch.catchAll` \e -> do
                let body = Http.RequestBodyBS $ Lib.ByteString.fromUtf8String $ show e
                runtimeInvocationRepository.postError body
