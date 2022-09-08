module Lib.Observable.Main where

import Prelude
import Effect as Effect
import Effect.Now as Effect.Now
import Effect.Console as Effect.Console
import Effect.Class as Effect.Class
import Effect.Ref as Effect.Ref
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Tuple as Tuple
import Data.DateTime.Instant as Instant
import Control.Applicative as Applicative
import Control.Monad.Trans.Class as Trans
import Control.Monad.Cont.Trans as ContT
import Control.Monad.Maybe.Trans as MaybeT
import Control.MonadZero as Control.MonadZero
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Web.HTML.Window as Web.HTML.Window
import Lib.Window.Main as Window
import Lib.Subscription.Main as Subscription

-- (a -> Effect.Effect ()) -> Effect.Effect ()
newtype Observable a = Observable (Unwrapped a)

type Unwrapped a = (a -> Effect.Effect Unit) -> Effect.Effect Subscription.Subscription

instance newtypeObservable :: Newtype.Newtype (Observable a) (Unwrapped a)
instance functorObservable :: Functor Observable where
  map fn ma = Observable $ \cb -> do
    Newtype.unwrap ma $ \a -> cb (fn a)
instance applyObservable :: Apply Observable where
  apply mfn ma = do
    fn <- mfn
    a <- ma
    pure $ fn a
instance applicativeObservable :: Applicative Observable where
  pure a = Observable $ \ca -> do
    ca a
    pure Subscription.empty
instance bindObservable :: Bind Observable where
  bind ma fn = switchMap fn ma
instance semigroupObservable :: Semigroup (Observable a) where
  append mx my = concat [mx, my]
instance monadObservable :: Monad Observable
instance monadEffectObservable :: Effect.Class.MonadEffect Observable where
  liftEffect ma = Observable $ \ca -> do
    a <- ma
    ca a
    pure Subscription.empty
instance monadContObservable :: ContT.MonadCont Observable where
  callCC = call

subscribe :: forall a. Observable a -> (a -> Effect.Effect Unit) -> Effect.Effect Subscription.Subscription
subscribe = Newtype.unwrap

call :: forall a b. ((a -> Observable b) -> Observable a) -> Observable a
call fn = Observable $ \ca -> do
  saRef <- Effect.Ref.new Subscription.empty
  sa <- flip Newtype.unwrap ca $ fn $ \a -> do
    Effect.Class.liftEffect $ do
      sa <- Effect.Ref.read saRef
      Subscription.unsubscribe sa
    Observable $ \_ -> do
      ca a
      pure Subscription.empty
  Effect.Ref.write sa saRef
  pure sa

switch :: forall a. Observable (Observable a) -> Observable a
switch mma = Observable $ \ca -> do
  ref <- Effect.Ref.new Subscription.empty
  sa <- Newtype.unwrap mma $ \ma -> do
    prev <- Effect.Ref.read ref
    Subscription.unsubscribe prev
    next <- Newtype.unwrap ma ca
    Effect.Ref.write next ref
  pure $ Newtype.wrap do
    sb <- Effect.Ref.read ref
    Subscription.unsubscribe (sa <> sb)

switchMap :: forall a b. (a -> Observable b) -> Observable a -> Observable b
switchMap fn ma = switch $ fn <$> ma

merge :: forall a. Observable (Observable a) -> Observable a
merge mma = Observable $ \ca -> do
  ref <- Effect.Ref.new Subscription.empty
  sa <- Newtype.unwrap mma $ \ma -> do
    next <- Newtype.unwrap ma ca
    void $ Effect.Ref.modify ((<>) next) ref
  pure $ Newtype.wrap do
    sb <- Effect.Ref.read ref
    Subscription.unsubscribe (sa <> sb)

mergeMap :: forall a b. (a -> Observable b) -> Observable a -> Observable b
mergeMap fn ma = merge $ fn <$> ma

replace :: forall a. Observable (Observable a) -> Observable a
replace = merge <<< take 1

replaceMap :: forall a b. (a -> Observable b) -> Observable a -> Observable b
replaceMap fn ma = replace $ fn <$> ma

toCont :: forall a. Observable a -> ContT.ContT Unit Effect.Effect a
toCont ma = ContT.ContT $ \ca -> do
  void $ Newtype.unwrap ma ca

fromCont :: forall a. ContT.ContT Unit Effect.Effect a -> Observable a
fromCont ma = Observable $ \ca -> do
  _ <- ContT.runContT ma ca
  pure Subscription.empty

fromEvent :: Web.Event.Event.EventType -> Web.Event.EventTarget.EventTarget -> Observable Web.Event.Internal.Types.Event
fromEvent eventType target = Observable $ \ce -> do
  listener <- Web.Event.EventTarget.eventListener ce
  Web.Event.EventTarget.addEventListener eventType listener false target
  pure $ Newtype.wrap do
    Web.Event.EventTarget.removeEventListener eventType listener false target

fromAnimationLoop :: Web.HTML.Window.Window -> Observable Number
fromAnimationLoop window = Observable $ \ca -> do
  Newtype.wrap <$> Window.requestAnimationLoop ca window

on :: forall a b. Observable b -> Observable a -> Observable (Tuple.Tuple a b)
on mb ma = Observable $ \cab -> do
  bRef <- Effect.Ref.new Maybe.Nothing
  sa <- Newtype.unwrap ma $ \a -> do
    void $ MaybeT.runMaybeT do
      b <- MaybeT.MaybeT $ Effect.Ref.read bRef
      Trans.lift $ cab (Tuple.Tuple a b)
  sb <- Newtype.unwrap mb $ \b -> do
    Effect.Ref.write (pure b) bRef
  pure $ sa <> sb

fold :: forall a b. (a -> b -> b) -> b -> Observable a -> Observable b
fold step seed ma =
  concat
    [ do
        ref <- Effect.Class.liftEffect (Effect.Ref.new seed)
        a <- ma
        Observable $ \cb -> do
          acc <- Effect.Ref.modify (step a) ref
          cb acc
          pure Subscription.empty
    , pure seed
    ]

loop :: forall a. (a -> Observable a) -> a -> Observable a
loop step seed = Observable $ \ca -> do
  flip Newtype.unwrap ca $ do
    let iter x = replaceMap iter (tapIO ca $ step x)
    iter seed

expand :: forall a. (a -> Observable a) -> Observable a -> Observable a
expand step ma = Observable $ \ca -> do
  flip Newtype.unwrap ca $ do
    let iter x = replaceMap iter (tapIO ca $ step x)
    switchMap iter (tapIO ca ma)

take :: forall a. Int -> Observable a -> Observable a
take n ma = call $ \unsubscribe -> do
  countRef <- Effect.Class.liftEffect $ Effect.Ref.new 0
  a <- ma
  count <- Effect.Class.liftEffect $ Effect.Ref.modify ((+) 1) countRef
  Applicative.when (count >= n) (unsubscribe a)
  pure a

concat :: forall a. Array (Observable a) -> Observable a
concat mas = Observable $ \ca -> do
  ss <- Traversable.for mas $ \ma -> do
    Newtype.unwrap ma ca
  pure $ Subscription.concat ss

filter :: forall a. (a -> Boolean) -> Observable a -> Observable a
filter fn ma = Observable $ \ca -> do
  Newtype.unwrap ma $ \a -> do
    Applicative.when (fn a) (ca a)

filterMap :: forall a b. (a -> Maybe.Maybe b) -> Observable a -> Observable b
filterMap fn ma = Observable $ \cb -> do
  Newtype.unwrap (fn <$> ma) $ Maybe.maybe (pure unit) cb

when :: forall a. Boolean -> Observable a -> Observable a
when bx mx = filter (const bx) mx

compact :: forall a. Observable (Maybe.Maybe a) -> Observable a
compact = filterMap identity

bindIO :: forall a b. (a -> Effect.Effect b) -> Observable a -> Observable b
bindIO fn mx = joinIO $ fn <$> mx

joinIO :: forall a. Observable (Effect.Effect a) -> Observable a
joinIO mea = mea >>= Effect.Class.liftEffect

tapIO :: forall a. (a -> Effect.Effect Unit) -> Observable a -> Observable a
tapIO fn ma = Observable $ \ca -> do
  Newtype.unwrap ma $ \a -> do
    fn a
    ca a

