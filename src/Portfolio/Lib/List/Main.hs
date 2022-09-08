module Portfolio.Lib.List.Main
  ( prependAll
  , filterMap
  ) where

import RIO
import qualified Data.Maybe as Maybe

prependAll :: a -> [a] -> [a]
prependAll d xs = xs >>= \x -> [d,x]

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = Maybe.mapMaybe
