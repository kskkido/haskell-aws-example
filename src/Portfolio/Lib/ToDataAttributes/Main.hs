module Portfolio.Lib.ToDataAttributes.Main
  ( DataAttributes
  , ToDataAttributes(..)
  ) where

import RIO
import qualified RIO.Map as Map

type DataAttributes = Map.Map Text Text

class ToDataAttributes a where
  encode :: a -> DataAttributes
