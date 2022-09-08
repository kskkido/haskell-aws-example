module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeInvocationContext.Main
  ( LambdaRuntimeInvocationContext(..)
  , HasLambdaRuntimeInvocationContext(..)
  , fromRuntime
  , toLambdaHandlerInput
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.IO.Class as MonadIO
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.LambdaRuntimeInvocation.Main as LambdaRuntimeInvocation
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.App.Main as App
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandler.Main as LambdaHandler
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerContext.Main as LambdaHandlerContext
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerEvent.Main as LambdaHandlerEvent
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerInput.Main as LambdaHandlerInput
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeContext.Main as LambdaRuntimeContext
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeEnvironment.Main as LambdaRuntimeEnvironment
import qualified Portfolio.Lib.AwsLambdaRuntime.Repositories.LambdaRuntimeInvocation.Data.Repository.Main as LambdaRuntimeInvocation.Repository


data LambdaRuntimeInvocationContext = LambdaRuntimeInvocationContext
  { requestUuid :: Text.Text
  , handler :: LambdaHandler.LambdaHandler
  , handlerEvent :: LambdaHandlerEvent.LambdaHandlerEvent
  , handlerContext :: LambdaHandlerContext.LambdaHandlerContext
  , runtimeInvocation :: LambdaRuntimeInvocation.LambdaRuntimeInvocation
  , runtimeContext :: LambdaRuntimeContext.LambdaRuntimeContext
  , runtimeInvocationRepository :: LambdaRuntimeInvocation.Repository.Repository
  }
  deriving (Generic)

class HasLambdaRuntimeInvocationContext a where
  get :: a -> LambdaRuntimeInvocationContext
instance LambdaRuntimeContext.HasLambdaRuntimeContext LambdaRuntimeInvocationContext where
  get = runtimeContext

fromRuntime :: (LambdaRuntimeContext.HasLambdaRuntimeContext r, LambdaRuntimeEnvironment.HasLambdaRuntimeEnvironment r, MonadIO.MonadIO m) => App.App r m LambdaRuntimeInvocationContext
fromRuntime = do
  context <- Reader.asks LambdaRuntimeContext.get
  runtimeInvocation <- liftIO context.runtimeRepository.getNextInvocation
  handler <-
    ( ( LambdaRuntimeContext.toLambdaHandler context ) &
      ( App.fromMaybe "missing handler" )
    )
  handlerContext <- LambdaHandlerContext.fromRuntimeInvocation runtimeInvocation
  pure $ LambdaRuntimeInvocationContext
    { requestUuid = ""
    , handler = handler
    , handlerEvent = runtimeInvocation.handlerEvent
    , handlerContext = handlerContext
    , runtimeInvocation = runtimeInvocation
    , runtimeContext = context
    , runtimeInvocationRepository = LambdaRuntimeInvocation.Repository.fromAwsLambdaRuntimeApi
        context.runtimeApiQueryContext
        runtimeInvocation.awsRequestId
    }

toLambdaHandlerInput :: LambdaRuntimeInvocationContext -> LambdaHandlerInput.LambdaHandlerInput
toLambdaHandlerInput context = LambdaHandlerInput.LambdaHandlerInput
  { LambdaHandlerInput.event = context.handlerEvent
  , LambdaHandlerInput.context = context.handlerContext
  }
