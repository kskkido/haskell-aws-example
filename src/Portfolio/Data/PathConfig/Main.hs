module Portfolio.Data.PathConfig.Main
  ( PathConfig(..)
  , HasPathConfig(..)
  ) where

import RIO

data PathConfig = PathConfig
  { delimiter :: Char
  , parameter :: Char
  }

class HasPathConfig a where
  get :: a -> PathConfig

instance HasPathConfig PathConfig where
  get = id
