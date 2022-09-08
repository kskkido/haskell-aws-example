module Portfolio.Lib.FilePath.Main
  ( toDirectory
  , toRelativePath
  , toAbsolutePath
  , toCurrentRelativePath
  , toFile
  , toFileExtension
  , combine
  , matchExtension
  , modifyFile
  , stripFileExtension
  ) where

import RIO
import qualified RIO.List as List
import qualified RIO.FilePath as FilePath
import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.List.Split as List.Split

combine :: IO.FilePath -> IO.FilePath -> IO.FilePath
combine = FilePath.combine

toDirectory :: IO.FilePath -> IO.FilePath
toDirectory = FilePath.takeDirectory

toRelativePath :: IO.FilePath -> IO.FilePath -> IO.FilePath
toRelativePath = FilePath.makeRelative

toAbsolutePath :: IO.FilePath -> IO.IO IO.FilePath
toAbsolutePath = Directory.makeAbsolute

toCurrentRelativePath :: IO.FilePath -> IO.IO String
toCurrentRelativePath filePath = do
  directory <- Directory.getCurrentDirectory
  pure $ toRelativePath directory filePath

toFile :: IO.FilePath -> Maybe.Maybe IO.FilePath
toFile filePath = do
  let pathInfo = List.Split.splitOn "/" filePath
  List.lastMaybe pathInfo

toFileExtension :: IO.FilePath -> Maybe.Maybe IO.FilePath
toFileExtension filePath = do
  file <- toFile filePath
  let fileInfo = List.Split.splitOn "." file
  List.lastMaybe fileInfo

matchExtension :: String -> IO.FilePath -> Maybe.Maybe IO.FilePath
matchExtension extension filePath = do
  fileExtension <- toFileExtension filePath
  Control.Monad.guard (extension `List.isSuffixOf` fileExtension)
  pure filePath

modifyFile :: (String -> String) -> IO.FilePath -> IO.FilePath
modifyFile fn filePath = do
  let pathInfo = List.Split.splitOn "/" filePath
  maybe (fn filePath) (List.intercalate "/") do
    init <- List.initMaybe pathInfo
    last <- List.lastMaybe pathInfo
    pure $ init <> [fn last]

stripFileExtension :: IO.FilePath -> IO.FilePath
stripFileExtension filePath = flip modifyFile filePath \file ->
  fromMaybe file do
    let fileInfo = List.Split.splitOn "." file
    init <- List.initMaybe fileInfo
    pure $ List.intercalate "." init

