module Portfolio.Applications.Cli.Data.CliInput.Main
  ( CliInput(..)
  , fromCli
  , parseFromCli
  ) where

import RIO
import qualified Data.Foldable as Foldable
import qualified Options.Applicative as Options
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import qualified Portfolio.Applications.LambdaPackager.Data.AppInput.Main as LambdaPackager.AppInput

data CliInput =
    CliStaticSiteGeneratorInput
  | CliDeployRoleStackInput
  | CliDeployStackInput
  | CliDevServerInput
  | CliLambdaPackagerInput LambdaPackager.AppInput.AppInput
  deriving (Eq, Show, Generic)

fromCli :: IO.IO CliInput
fromCli = Options.execParser $ Options.info parseFromCli mempty

parseFromCli :: Options.Parser CliInput
parseFromCli =
  Foldable.asum
    [ ( ( Options.command "static-site-generator"
          ( Options.info
            ( pure CliStaticSiteGeneratorInput )
            mempty
          )
        ) &
        ( Options.subparser )
      )
    , ( ( Options.command "deploy-role-stack"
          ( Options.info
            ( pure CliDeployRoleStackInput )
            mempty
          )
        ) &
        ( Options.subparser )
      )
    , ( ( Options.command "deploy-stack"
          ( Options.info
            ( pure CliDeployStackInput )
            mempty
          )
        ) &
        ( Options.subparser )
      )
    , ( ( Options.command "dev-server"
          ( Options.info
            ( pure CliDevServerInput )
            mempty
          )
        ) &
        ( Options.subparser )
      )
    , ( ( Options.command "lambda-packager"
          ( Options.info
            ( LambdaPackager.AppInput.AppCopyInput
              <$> ( ( Options.strOption ) $
                    ( Options.long "input" <> Options.short 'i' )
                  )
              <*> ( ( Options.strOption ) $
                    ( Options.long "output" <> Options.short 'o' )
                  )
            )
            mempty
          )
        ) &
        ( Options.subparser ) <&>
        ( CliLambdaPackagerInput )
      )
    ]
