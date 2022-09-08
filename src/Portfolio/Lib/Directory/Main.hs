module Portfolio.Lib.Directory.Main
  ( copyDirectory
  , writeFile
  , removeDirectory
  , listFiles
  ) where

import RIO
import qualified RIO.FilePath as FilePath
import qualified System.IO as IO
import qualified System.Directory as Directory
import qualified Control.Monad.Extra as Extra

writeFile :: IO.FilePath -> String -> IO.IO ()
writeFile path file = do
  let directory = FilePath.takeDirectory path
  IO.print directory
  Directory.createDirectoryIfMissing True directory
  IO.writeFile path file

copyDirectory :: String -> String -> IO.IO [IO.FilePath]
copyDirectory source destination = do
  Directory.createDirectoryIfMissing True destination
  paths <- Directory.listDirectory source
  flip foldMap paths $ \path -> do
    let sourcePath = source <> "/" <> path
        destinationPath =  destination <> "/" <> path
    Extra.ifM
      ( Directory.doesDirectoryExist sourcePath )
      ( copyDirectory sourcePath destinationPath )
      ( Directory.copyFile sourcePath destinationPath $> [destinationPath] )

removeDirectory :: IO.FilePath -> IO.IO ()
removeDirectory filePath = do
  Extra.whenM (Directory.doesDirectoryExist filePath) do
    Directory.removeDirectoryRecursive filePath

listFiles :: String -> IO.IO [IO.FilePath]
listFiles source = do
  paths <- Directory.listDirectory source
  flip foldMap paths $ \path -> do
    let sourcePath = source <> "/" <> path
    Extra.ifM
      ( Directory.doesDirectoryExist sourcePath )
      ( listFiles sourcePath )
      ( pure [sourcePath] )
