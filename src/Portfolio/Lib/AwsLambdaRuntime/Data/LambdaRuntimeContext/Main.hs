module Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeContext.Main
  ( LambdaRuntimeContext(..)
  , HasLambdaRuntimeContext(..)
  , fromInput
  , fromRuntime
  , toLambdaHandler
  , refresh
  ) where

import RIO
import qualified RIO.HashMap as HashMap
import qualified Data.Maybe as Maybe
import qualified System.IO.Error as IO.Error
import qualified Network.HTTP.Client as HTTP
import qualified Control.Monad as Monad
import qualified Portfolio.Lib.HashMap.Main as HashMap
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.QueryContext.Main as AwsLambdaRuntimeApi.QueryContext
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandler.Main as LambdaHandler
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerMap.Main as LambdaHandlerMap
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeInput.Main as LambdaRuntimeInput
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeEnvironment.Main as LambdaRuntimeEnvironment
import qualified Portfolio.Lib.AwsLambdaRuntime.Repositories.LambdaRuntime.Data.Repository.Main as LambdaRuntime.Repository

data LambdaRuntimeContext = LambdaRuntimeContext
  { environment :: LambdaRuntimeEnvironment.LambdaRuntimeEnvironment
  , handlerMap :: LambdaHandlerMap.LambdaHandlerMap
  , httpManager :: HTTP.Manager
  , runtimeApiQueryContext :: AwsLambdaRuntimeApi.QueryContext.QueryContext
  , runtimeRepository :: LambdaRuntime.Repository.Repository
  , input :: LambdaRuntimeInput.LambdaRuntimeInput
  }
  deriving (Generic)

class HasLambdaRuntimeContext a where
  get :: a -> LambdaRuntimeContext
instance HasLambdaRuntimeContext LambdaRuntimeContext where
  get = id
instance AwsLambdaRuntimeApi.QueryContext.HasQueryContext LambdaRuntimeContext where
  get = runtimeApiQueryContext
instance LambdaRuntimeEnvironment.HasLambdaRuntimeEnvironment LambdaRuntimeContext where
  get = environment

fromInput :: MonadIO m => LambdaRuntimeInput.LambdaRuntimeInput -> m LambdaRuntimeContext
fromInput input = do
  environment <- liftIO
    ( ( LambdaRuntimeEnvironment.fromSystem ) >>=
      ( either (IO.Error.ioError . IO.Error.userError) pure )
    )
  httpManager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  let runtimeApiQueryContext = AwsLambdaRuntimeApi.QueryContext.QueryContext
        { AwsLambdaRuntimeApi.QueryContext.host = environment.awsLambdaRuntimeApiSource.host
        , AwsLambdaRuntimeApi.QueryContext.port = environment.awsLambdaRuntimeApiSource.port
        , AwsLambdaRuntimeApi.QueryContext.version = environment.awsLambdaRuntimeApiVersion
        , AwsLambdaRuntimeApi.QueryContext.httpManager = httpManager
        }
      runtimeRepository = LambdaRuntime.Repository.fromAwsLambdaRuntimeApi runtimeApiQueryContext
  pure $ LambdaRuntimeContext
    { input = input
    , environment = environment
    , handlerMap = input.handlerMap
    , httpManager = httpManager
    , runtimeApiQueryContext = runtimeApiQueryContext
    , runtimeRepository = runtimeRepository
    }

fromRuntime :: MonadIO m => m LambdaRuntimeContext
fromRuntime = do
  environment <- liftIO
    ( ( LambdaRuntimeEnvironment.fromSystem ) >>=
      ( either (IO.Error.ioError . IO.Error.userError) pure )
    )
  httpManager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  let runtimeApiQueryContext = AwsLambdaRuntimeApi.QueryContext.QueryContext
        { AwsLambdaRuntimeApi.QueryContext.host = environment.awsLambdaRuntimeApiSource.host
        , AwsLambdaRuntimeApi.QueryContext.port = environment.awsLambdaRuntimeApiSource.port
        , AwsLambdaRuntimeApi.QueryContext.version = environment.awsLambdaRuntimeApiVersion
        , AwsLambdaRuntimeApi.QueryContext.httpManager = httpManager
        }
      runtimeRepository = LambdaRuntime.Repository.fromAwsLambdaRuntimeApi runtimeApiQueryContext
  pure $ LambdaRuntimeContext
    { input = LambdaRuntimeInput.empty
    , environment = environment
    , handlerMap = mempty
    , httpManager = httpManager
    , runtimeApiQueryContext = runtimeApiQueryContext
    , runtimeRepository = runtimeRepository
    }

toLambdaHandler :: LambdaRuntimeContext -> Maybe.Maybe LambdaHandler.LambdaHandler
toLambdaHandler context =
  Monad.msum
    [ HashMap.toValue context.handlerMap
    , HashMap.lookup context.environment.handlerName context.handlerMap
    ]

refresh :: MonadIO m => LambdaRuntimeContext -> m LambdaRuntimeContext
refresh context = do
  environment <- liftIO
    ( ( LambdaRuntimeEnvironment.fromSystem ) >>=
      ( either (IO.Error.ioError . IO.Error.userError) pure )
    )
  let runtimeApiQueryContext = AwsLambdaRuntimeApi.QueryContext.QueryContext
        { AwsLambdaRuntimeApi.QueryContext.host = environment.awsLambdaRuntimeApiSource.host
        , AwsLambdaRuntimeApi.QueryContext.port = environment.awsLambdaRuntimeApiSource.port
        , AwsLambdaRuntimeApi.QueryContext.version = environment.awsLambdaRuntimeApiVersion
        , AwsLambdaRuntimeApi.QueryContext.httpManager = context.httpManager
        }
      runtimeRepository = LambdaRuntime.Repository.fromAwsLambdaRuntimeApi runtimeApiQueryContext
  pure $ context
    { environment = environment
    , runtimeApiQueryContext = runtimeApiQueryContext
    , runtimeRepository = runtimeRepository
    }

