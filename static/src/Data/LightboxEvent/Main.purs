module Data.LightboxEvent.Main where

import Prelude
import Effect as Effect
import Data.Maybe as Maybe
import Lib.KeyboardAction.Main as KeyboardAction

data LightboxEvent = Open String (Array String)| Close | Left | Right

instance showLightboxEvent :: Show LightboxEvent where
  show (Open _ _) = "Open"
  show Close       = "Close"
  show Left        = "Left"
  show Right       = "Right"

fromKeyboardAction :: KeyboardAction.KeyboardAction -> Maybe.Maybe LightboxEvent
fromKeyboardAction KeyboardAction.Left = pure Left
fromKeyboardAction KeyboardAction.Right = pure Right
fromKeyboardAction KeyboardAction.Up = pure Close
fromKeyboardAction KeyboardAction.Escape = pure Close
