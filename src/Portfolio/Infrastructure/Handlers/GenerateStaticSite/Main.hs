module Portfolio.Infrastructure.Handlers.GenerateStaticSite.Main
  ( handler
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Amazonka
import qualified Data.Aeson as Aeson
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Except as Except
import qualified Portfolio.Lib.Aeson.Main as Lib.Aeson
import qualified Portfolio.Lib.Either.Main as Lib.Either
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandler.Main as LambdaHandler
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandlerApiGatewayResponse.Main as LambdaHandlerApiGatewayResponse
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.App.Main as App
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.AppContext.Main as AppContext
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.PutObject.Main as PutObject
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Data.Distribution.Main as Distribution
import qualified Portfolio.Applications.StaticSiteGenerator.Main as StaticSiteGenerator

handler :: LambdaHandler.LambdaHandlerInvocation LambdaHandlerApiGatewayResponse.LambdaHandlerApiGatewayResponse
handler = liftIO do
  context <- Lib.Either.liftFail $ Except.runExceptT do
    config <- AppConfig.fromSystem
    AppContext.fromConfig config
  App.exec context $ do
    build <- liftIO $ StaticSiteGenerator.main context.staticSiteGenerator
    objects <- PutObject.fromBuild build
    invalidation <- Distribution.invalidateAllFiles
    void $ liftIO $ Amazonka.runResourceT do
      traverse_ (Amazonka.send context.amazonka) objects
      Amazonka.send context.amazonka invalidation
    pure $ LambdaHandlerApiGatewayResponse.LambdaHandlerApiGatewayResponse
      { LambdaHandlerApiGatewayResponse.isBase64Encoded = False
      , LambdaHandlerApiGatewayResponse.statusCode = 200
      , LambdaHandlerApiGatewayResponse.headers = mempty
      , LambdaHandlerApiGatewayResponse.body = Lib.Aeson.toSerializedText build
      }
    `Catch.catchAll` \e -> do
      pure $ LambdaHandlerApiGatewayResponse.LambdaHandlerApiGatewayResponse
        { LambdaHandlerApiGatewayResponse.isBase64Encoded = False
        , LambdaHandlerApiGatewayResponse.statusCode = 400
        , LambdaHandlerApiGatewayResponse.headers = mempty
        , LambdaHandlerApiGatewayResponse.body = Lib.Aeson.toSerializedText $ Aeson.String $ Text.pack $ show e
        }
