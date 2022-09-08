module Portfolio.Applications.Client.Data.FilePathConfig.Main
  ( FilePathConfig(..)
  , HasFilePathConfig(..)
  ) where

import RIO

data FilePathConfig = FilePathConfig
  { styles :: String
  , public :: String
  }

class HasFilePathConfig a where
  get :: a -> FilePathConfig
instance HasFilePathConfig FilePathConfig where
  get = id

