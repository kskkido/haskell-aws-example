module Portfolio.Applications.LambdaPackager.Main
  ( main
  ) where

import RIO
import qualified RIO.List as List
import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified System.FilePath.Posix as FilePath.Posix
import qualified System.Process.Typed as Process
import qualified Portfolio.Lib.ExitCode.Main as Lib.ExitCode
import qualified Portfolio.Lib.Either.Main as Lib.Either
import qualified Portfolio.Lib.Parsec.Data.ParseError.Main as Lib.Parsec.ParseError
import qualified Portfolio.Lib.Ldd.Data.LddOutput.Main as LddOutput
import qualified Portfolio.Applications.LambdaPackager.Data.AppInput.Main as AppInput

main :: AppInput.AppInput -> IO.IO ()
main input = case input of
  AppInput.AppCopyInput{input=inputPath, output=outputPath} -> do
    let processInput = List.intercalate " " ["ldd", inputPath]
    (processExitCode, processOutput) <- Process.readProcessStdout (Process.shell processInput)
    Lib.ExitCode.liftFail (pure processExitCode)
    lddOutput <- Lib.Either.liftFail (first Lib.Parsec.ParseError.toString <$> LddOutput.fromOutput processOutput)
    Directory.createDirectoryIfMissing True outputPath
    forM_ (LddOutput.toSharedLibraries lddOutput) $ \library -> do
      let filename = FilePath.Posix.takeFileName library.filepath
      Directory.copyFileWithMetadata library.filepath (outputPath FilePath.Posix.</> filename)
