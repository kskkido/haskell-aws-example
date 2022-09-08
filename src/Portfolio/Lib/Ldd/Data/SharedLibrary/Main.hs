module Portfolio.Lib.Ldd.Data.SharedLibrary.Main
  ( SharedLibrary(..)
  ) where

import RIO

data SharedLibrary = SharedLibrary
  { filename :: String
  , filepath :: String
  , address :: String
  }
  deriving (Eq, Show, Generic)

