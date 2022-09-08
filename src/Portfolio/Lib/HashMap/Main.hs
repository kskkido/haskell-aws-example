module Portfolio.Lib.HashMap.Main
  ( toValue
  ) where

import RIO
import RIO.List as List
import qualified RIO.HashMap as HashMap
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable

toValue :: HashMap a b -> Maybe b
toValue hashMap = do
  let values = HashMap.elems hashMap
  Monad.guard (length values == 1)
  List.headMaybe values

