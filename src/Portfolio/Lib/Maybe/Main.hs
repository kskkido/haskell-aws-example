module Portfolio.Lib.Maybe.Main
  ( fromEither
  , partitionMaybe
  ) where

import RIO hiding (fromEither)
import qualified Data.Maybe as Maybe

fromEither :: Either a b -> Maybe.Maybe b
fromEither (Left _)  = Maybe.Nothing
fromEither (Right x) = Maybe.Just x

partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe fn = foldr step ([],[])
  where step x (ls,rs) = Maybe.maybe (ls,x:rs) (\y -> (y:ls,rs)) $ fn x

