module Portfolio.Data.Path.Main
  ( Path
  , toLocalizedPath
  , toRoutePattern
  , toLocalizedConfigPath
  , modifyFile
  , setExtension
  , setFile
  , toDirectory
  , prepend
  ) where

import RIO
import qualified RIO.List as List
import qualified Web.Scotty as Scotty
import qualified Control.Monad.Trans.Reader as Reader
import qualified Portfolio.Lib.Localize.Data.Config.Main as Localize.Config
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Localize.Locale
import qualified Portfolio.Data.PathInfo.Main as PathInfo
import qualified Portfolio.Data.PathConfig.Main as PathConfig

type Path = String

toLocalizedPath :: (PathConfig.HasPathConfig a, Monad m) => Localize.Locale.Locale -> Path -> Reader.ReaderT a m Path
toLocalizedPath locale path = PathInfo.fromPath path >>= PathInfo.toLocalizedPath locale

toLocalizedConfigPath :: (Localize.Config.HasConfig a, PathConfig.HasPathConfig a, Monad m) => Path -> Reader.ReaderT a m Path
toLocalizedConfigPath path = do
  config <- Reader.asks Localize.Config.get
  PathInfo.fromPath path >>= PathInfo.toLocalizedPath config.locale

toRoutePattern :: Path -> Scotty.RoutePattern
toRoutePattern = Scotty.literal

modifyFile :: (PathConfig.HasPathConfig a, Monad m) => (String -> String) -> Path -> Reader.ReaderT a m Path
modifyFile fn path = do
  pathInfo <- PathInfo.fromPath path
  maybe (pure $ fn path) PathInfo.toPath do
    init <- List.initMaybe pathInfo
    last <- List.lastMaybe pathInfo
    pure $ init <> [fn last]

setFile :: (PathConfig.HasPathConfig a, Monad m) => String -> Path -> Reader.ReaderT a m Path
setFile file = modifyFile $ const file

setExtension :: (PathConfig.HasPathConfig a, Monad m) => String -> Path -> Reader.ReaderT a m Path
setExtension extension = modifyFile \file -> (file <> "." <> extension)

toDirectory :: (PathConfig.HasPathConfig a, Monad m) => Path -> Reader.ReaderT a m Path
toDirectory path = do
  pathInfo <- PathInfo.fromPath path
  fromMaybe (pure "") do
    init <- List.initMaybe pathInfo
    pure $ PathInfo.toPath init

prepend :: (PathConfig.HasPathConfig a, Monad m) => String -> Path -> Reader.ReaderT a m Path
prepend prefix path = do
  prefixPathInfo <- PathInfo.fromPath prefix
  pathInfo <- (prefixPathInfo ++) <$> PathInfo.fromPath path
  PathInfo.toPath pathInfo
