module Lib.Subscription.Main where

import Prelude
import Effect as Effect
import Data.Newtype as Newtype
import Data.Foldable as Foldable

newtype Subscription = Subscription Unwrapped

type Unwrapped = Effect.Effect Unit

instance newtypeSubscription :: Newtype.Newtype Subscription Unwrapped
instance semigroupSubscription :: Semigroup Subscription where
  append ma mb = Newtype.wrap do
     _ <- Newtype.unwrap ma
     _ <- Newtype.unwrap mb
     pure unit

unsubscribe :: Subscription -> Effect.Effect Unit
unsubscribe = Newtype.unwrap

empty :: Subscription
empty = Newtype.wrap $ pure unit

concat :: Array Subscription -> Subscription
concat ss = Foldable.foldl (<>) empty ss
