module Portfolio.Applications.StaticSiteGenerator.Data.Build.Main
  ( Build(..)
  , from
  ) where

import RIO
import qualified Data.Aeson as Aeson
import qualified System.IO as IO
import qualified Portfolio.Lib.FilePath.Main as Lib.FilePath
import qualified Portfolio.Applications.StaticSiteGenerator.Data.File.Main as File

data Build = Build
  { filePath :: IO.FilePath
  , files :: [IO.FilePath]
  }
  deriving (Generic, Aeson.FromJSON, Aeson.ToJSON)

from :: IO.FilePath -> [File.File] -> Build
from directory fs = Build
  { filePath = directory
  , files = fs <&> Lib.FilePath.toRelativePath directory . File.toFilePath
  }

