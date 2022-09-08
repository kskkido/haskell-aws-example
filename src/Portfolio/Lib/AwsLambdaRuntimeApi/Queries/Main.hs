module Portfolio.Lib.AwsLambdaRuntimeApi.Queries.Main
  ( getNextInvocation
  , postInvocationResponse
  , postInvocationError
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Control.Monad.Reader as Reader
import qualified Network.HTTP.Simple as HTTP
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.Query.Main as Query
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.QueryContext.Main as QueryContext
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.LambdaRuntimeInvocation.Main as LambdaRuntimeInvocation
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.PostLambdaRuntimeInvocationResponseParameters.Main as PostLambdaRuntimeInvocationResponseParameters
import qualified Portfolio.Lib.AwsLambdaRuntimeApi.Data.PostLambdaRuntimeInvocationErrorParameters.Main as PostLambdaRuntimeInvocationErrorParameters

getNextInvocation :: (QueryContext.HasQueryContext a) => Query.Query a LambdaRuntimeInvocation.LambdaRuntimeInvocation
getNextInvocation = do
  context <- Reader.asks QueryContext.get
  request <-
    ( ( pure $ QueryContext.toRequest context ) <&>
      ( HTTP.setRequestMethod "GET" ) <&>
      ( HTTP.setRequestPath (encodeUtf8 $ Text.pack $ QueryContext.toPath context <> "/runtime/invocation/next") )
    )
  response <- HTTP.httpBS request
  Query.fromEither $ LambdaRuntimeInvocation.fromResponse response

postInvocationResponse :: (QueryContext.HasQueryContext a) => PostLambdaRuntimeInvocationResponseParameters.PostLambdaRuntimeInvocationResponseParameters -> Query.Query a ()
postInvocationResponse parameters = do
  context <- Reader.asks QueryContext.get
  request <-
    ( ( pure $ QueryContext.toRequest context ) <&>
      ( HTTP.setRequestMethod "POST" ) <&>
      ( HTTP.setRequestPath (encodeUtf8 $ Text.pack $ QueryContext.toPath context <> "/runtime/invocation/" <> parameters.requestId <> "/response") ) <&>
      ( HTTP.setRequestBody parameters.body )
    )
  void $ HTTP.httpBS request

postInvocationError :: (QueryContext.HasQueryContext a) => PostLambdaRuntimeInvocationErrorParameters.PostLambdaRuntimeInvocationErrorParameters -> Query.Query a ()
postInvocationError parameters = do
  context <- Reader.asks QueryContext.get
  request <-
    ( ( pure $ QueryContext.toRequest context ) <&>
      ( HTTP.setRequestMethod "POST" ) <&>
      ( HTTP.setRequestPath (encodeUtf8 $ Text.pack $ QueryContext.toPath context <> "/runtime/invocation/" <> parameters.requestId <> "/error") ) <&>
      ( HTTP.setRequestBody parameters.body )
    )
  void $ HTTP.httpBS request

