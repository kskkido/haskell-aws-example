module Portfolio.Applications.DevServer.Main
  ( main
  ) where

import RIO
import qualified RIO.Text.Lazy as Text.Lazy
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified Web.Scotty as Scotty
import qualified Network.Wai.Middleware.Cors as Cors
import qualified Network.Wai.Middleware.Static as Static
import qualified Portfolio.Lib.List.Main as Lib.List
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Locale
import qualified Portfolio.Lib.Middleware.Data.StaticPolicy.Main as Middleware.StaticPolicy
import qualified Portfolio.Applications.DevServer.Data.AppConfig.Main as AppConfig
import qualified Portfolio.Applications.StaticSiteGenerator.Main as StaticSiteGenerator

main :: IO.IO ()
main = do
  config <-
    ( ( AppConfig.fromSystem ) >>=
      ( either (IO.Error.ioError . IO.Error.userError) pure )
    )
  build <- do
    let staticSiteGeneratorConfig = AppConfig.toStaticSiteGeneratorAppConfig config
    StaticSiteGenerator.main staticSiteGeneratorConfig
  let redirectPaths =
        ( ( Lib.List.filterMap (Lib.FilePath.matchExtension "html") build.files ) &
          ( fmap Lib.FilePath.stripFileExtension >>= zip )
        )
  buildPath <- Lib.FilePath.toCurrentRelativePath build.filePath
  Scotty.scotty config.serverPort do
    Scotty.middleware Cors.simpleCors
    Scotty.middleware $ Static.staticPolicy $ fold
      [ Middleware.StaticPolicy.fromRedirectPaths redirectPaths Static.<|> Static.policy pure
      , Static.addBase buildPath
      ]
    Scotty.get "/" do
      Scotty.redirect $ Text.Lazy.pack $ Locale.toString config.defaultLocale
