module Portfolio.Applications.Cli.Main
  ( main
  ) where

import RIO
import qualified RIO.List as List
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath.Posix
import qualified System.Process.Typed as Process
import qualified Portfolio.Lib.ExitCode.Main as Lib.ExitCode
import qualified Portfolio.Lib.Either.Main as Lib.Either
import qualified Portfolio.Lib.Parsec.Data.ParseError.Main as Lib.Parsec.ParseError
import qualified Portfolio.Lib.Ldd.Data.LddOutput.Main as LddOutput
import qualified Portfolio.Applications.Cli.Data.CliInput.Main as CliInput
import qualified Portfolio.Applications.DevServer.Main as DevServer
import qualified Portfolio.Applications.DeployRoleStack.Main as DeployRoleStack
import qualified Portfolio.Applications.DeployStack.Main as DeployStack
import qualified Portfolio.Applications.LambdaPackager.Main as LambdaPackager
import qualified Portfolio.Applications.StaticSiteGenerator.Main as StaticSiteGenerator
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppConfig.Main as StaticSiteGenerator.AppConfig

main :: IO.IO ()
main = do
  cliInput <- CliInput.fromCli
  case cliInput of
    CliInput.CliStaticSiteGeneratorInput -> do
      config <- StaticSiteGenerator.AppConfig.fromSystem >>= either (IO.Error.ioError . IO.Error.userError) return
      void $ StaticSiteGenerator.main config
    CliInput.CliDeployRoleStackInput -> do
      DeployRoleStack.main
    CliInput.CliDeployStackInput -> do
      DeployStack.main
    CliInput.CliDevServerInput -> do
      DevServer.main
    CliInput.CliLambdaPackagerInput input -> do
      LambdaPackager.main input
