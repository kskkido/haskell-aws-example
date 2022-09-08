module Lib.Window.Main where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Data.Maybe as Maybe
import Control.Monad.Trans.Class as Trans
import Control.Monad.Maybe.Trans as MaybeT
import Web.HTML.Window as Web.HTML.Window

foreign import requestAnimationFrameImpl :: (Number -> Effect.Effect Unit) -> Web.HTML.Window.Window -> Effect.Effect Web.HTML.Window.RequestAnimationFrameId
foreign import cancelAnimationFrameImpl :: Web.HTML.Window.RequestAnimationFrameId -> Web.HTML.Window.Window -> Effect.Effect Unit

requestAnimationFrame :: (Number -> Effect.Effect Unit) -> Web.HTML.Window.Window -> Effect.Effect Web.HTML.Window.RequestAnimationFrameId
requestAnimationFrame = requestAnimationFrameImpl

cancelAnimationFrame :: Web.HTML.Window.RequestAnimationFrameId -> Web.HTML.Window.Window -> Effect.Effect Unit
cancelAnimationFrame = cancelAnimationFrameImpl

requestAnimationLoop :: (Number -> Effect.Effect Unit) -> Web.HTML.Window.Window -> Effect.Effect (Effect.Effect Unit)
requestAnimationLoop fn window = do
  rafIdRef <- Effect.Ref.new Maybe.Nothing
  startRef <- Effect.Ref.new Maybe.Nothing
  let iter = do
        rafId <- flip requestAnimationFrame window $ \timestamp -> do
          start <- Maybe.fromMaybe timestamp <$> Effect.Ref.read startRef
          fn (timestamp - start)
          iter
        Effect.Ref.write (pure rafId) rafIdRef
      init = do
        rafId <- flip requestAnimationFrame window $ \timestamp -> do
          Effect.Ref.write (pure timestamp) startRef
          fn 0.0
          iter
        Effect.Ref.write (pure rafId) rafIdRef
  init
  pure do
    void $ MaybeT.runMaybeT do
      rafId <- MaybeT.MaybeT $ Effect.Ref.read rafIdRef
      Trans.lift $ cancelAnimationFrame rafId window

