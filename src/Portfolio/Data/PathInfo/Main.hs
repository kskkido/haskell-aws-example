module Portfolio.Data.PathInfo.Main
  ( PathInfo
  , fromPath
  , toPath
  , toLocalized
  , toLocalizedPath
  ) where

import RIO
import qualified RIO.List as List
import qualified Control.Monad
import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as List.Split
import qualified Portfolio.Lib.List.Main as List
import qualified Portfolio.Lib.Localize.Data.Locale.Main as Localize.Locale
import qualified Portfolio.Data.PathConfig.Main as PathConfig

type PathInfo = [String]

fromPath :: (PathConfig.HasPathConfig a, Monad m) => String -> Reader.ReaderT a m PathInfo
fromPath xs = do
  config <- Reader.asks PathConfig.get
  pure $ filter (not . null) $ List.Split.splitOn [config.delimiter] xs

toPath :: (PathConfig.HasPathConfig a, Monad m) => PathInfo -> Reader.ReaderT a m String
toPath px = do
  config <- Reader.asks PathConfig.get
  pure $ Maybe.maybe [config.delimiter] (join . List.prependAll [config.delimiter]) do
    Control.Monad.guard (List.length px > 0)
    pure px

toLocalized :: Localize.Locale.Locale -> PathInfo -> PathInfo
toLocalized locale info = Maybe.fromMaybe ([show locale] <> info) $ do
  (x, xs) <- List.uncons info
  Control.Monad.guard (Maybe.isJust $ Localize.Locale.fromString x)
  pure $ [show locale] <> xs

toLocalizedPath :: (PathConfig.HasPathConfig a, Monad m) => Localize.Locale.Locale -> PathInfo -> Reader.ReaderT a m String
toLocalizedPath locale info = toLocalized locale info & toPath

