module Portfolio.Lib.Function.Main
  ( shiftA3
  ) where

import RIO

shiftA3 :: (a -> b -> c -> d) -> b -> c -> a -> d
shiftA3 fn b c a = fn a b c
