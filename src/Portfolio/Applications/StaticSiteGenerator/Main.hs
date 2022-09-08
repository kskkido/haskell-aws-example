module Portfolio.Applications.StaticSiteGenerator.Main
  ( main
  ) where

import RIO
import qualified System.IO as IO
import qualified Portfolio.Lib.Directory.Main as Lib.Directory
import qualified Portfolio.Applications.Client.Main as Client
import qualified Portfolio.Applications.StaticSiteGenerator.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Applications.StaticSiteGenerator.Data.File.Main as File
import qualified Portfolio.Applications.StaticSiteGenerator.Data.Build.Main as Build

main :: AppConfig.AppConfig -> IO.IO Build.Build
main config = do
  Lib.Directory.removeDirectory config.filePathBuild
  files <-
    fold
      [ do
          let clientAppConfig = AppConfig.toClientAppConfig config
          pages <- Client.main clientAppConfig
          for pages $ File.fromPage config.filePathBuild
      , do
          filePaths <- Lib.Directory.copyDirectory config.filePathStatic config.filePathBuild
          pure $ filePaths <&> File.File
      ]
  pure $ Build.from config.filePathBuild files
