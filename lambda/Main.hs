module Main
  ( main
  ) where

import RIO
import qualified RIO.HashMap as HashMap
import qualified System.IO as IO
import qualified Portfolio.Lib.AwsLambdaRuntime.Main as AwsLambdaRuntime
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaRuntimeInput.Main as AwsLambdaRuntime.LambdaRuntimeInput
import qualified Portfolio.Lib.AwsLambdaRuntime.Data.LambdaHandler.Main as LambdaHandler
import qualified Portfolio.Infrastructure.Handlers.GenerateStaticSite.Main as Handlers.GenerateStaticSite

main :: IO.IO ()
main = do
  AwsLambdaRuntime.run $ AwsLambdaRuntime.LambdaRuntimeInput.LambdaRuntimeInput
    { handlerMap = HashMap.fromList
        [ ("generateStaticSite", LambdaHandler.fromInvocation $ Handlers.GenerateStaticSite.handler)
        ]
    }
