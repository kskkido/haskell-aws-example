module Portfolio.Lib.Contentful.Data.Entries.Main
  ( Entries(..)
  ) where

import RIO
import qualified Portfolio.Lib.Contentful.Data.Item.Main as Item
import qualified Portfolio.Lib.Contentful.Data.Asset.Main as Asset

data Entries = Entries
  { asset :: Asset.Asset
  , items :: [Item.Item]
  }
  deriving (Eq, Show)
