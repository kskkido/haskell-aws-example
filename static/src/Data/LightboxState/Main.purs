module Data.LightboxState.Main where

import Prelude
import Control.MonadZero as MonadZero
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Tuple.Nested as Tuple.Nested
import Data.LightboxActiveState.Main as LightboxActiveState
import Data.LightboxInactiveState.Main as LightboxInactiveState
import Data.LightboxEvent.Main as LightboxEvent
import Lib.Array.Main as Array

data LightboxState =
    Active LightboxActiveState.LightboxActiveState
  | Inactive LightboxInactiveState.LightboxInactiveState

instance showLightboxState :: Show LightboxState where
  show (Active _ ) = "Active"
  show (Inactive _) = "Inactive"

unit :: LightboxState
unit = Inactive LightboxInactiveState.LightboxInactiveState

foldEvent :: LightboxEvent.LightboxEvent -> LightboxState -> LightboxState
foldEvent (LightboxEvent.Open x xs) state =
  Active $ LightboxActiveState.LightboxActiveState
    { id: x
    , ids: xs
    }
foldEvent LightboxEvent.Close state =
  Inactive $ LightboxInactiveState.LightboxInactiveState
foldEvent LightboxEvent.Left state = Maybe.fromMaybe state do
  (LightboxActiveState.LightboxActiveState activeState) <- toActiveState state
  next <- Array.before (activeState.id) activeState.ids
  pure $ foldEvent (LightboxEvent.Open next activeState.ids) state
foldEvent LightboxEvent.Right state = Maybe.fromMaybe state do
  (LightboxActiveState.LightboxActiveState activeState) <- toActiveState state
  next <- Array.after (activeState.id) activeState.ids
  pure $ foldEvent (LightboxEvent.Open next activeState.ids) state

toActiveState :: LightboxState -> Maybe.Maybe LightboxActiveState.LightboxActiveState
toActiveState (Active state) = pure state
toActiveState (Inactive _) = Maybe.Nothing

active :: LightboxState -> Boolean
active = toActiveState >>> Maybe.isJust
