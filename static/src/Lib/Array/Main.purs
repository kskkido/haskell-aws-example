module Lib.Array.Main where

import Prelude
import Control.MonadZero as MonadZero
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple.Nested

lookaround :: forall a. Array a -> Array (Tuple.Nested.Tuple3 (Maybe.Maybe a) a (Maybe.Maybe a))
lookaround xs =
  let pxs = Maybe.Nothing `Array.cons` (pure <$> xs)
      nxs = (pure <$> (Maybe.fromMaybe [] $ Array.tail xs)) <> [Maybe.Nothing]
      axs = Array.zip pxs nxs
   in Array.zipWith (\curr around -> Tuple.Nested.tuple3 (Tuple.fst around) curr (Tuple.snd around)) xs axs

before :: forall a. Eq a => a -> Array a -> Maybe.Maybe a
before x xs = flip Array.findMap (lookaround xs) $ \ax -> do
  MonadZero.guard (x == Tuple.Nested.get2 ax)
  Tuple.Nested.get1 ax

after :: forall a. Eq a => a -> Array a -> Maybe.Maybe a
after x xs = flip Array.findMap (lookaround xs) $ \ax -> do
  MonadZero.guard (x == Tuple.Nested.get2 ax)
  Tuple.Nested.get3 ax
