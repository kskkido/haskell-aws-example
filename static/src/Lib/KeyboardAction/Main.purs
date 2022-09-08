module Lib.KeyboardAction.Main where

import Prelude
import Data.Maybe as Maybe
import Data.Generic.Rep as Generic.Rep
import Data.Show.Generic as Show.Generic
import Control.MonadPlus as Control.MonadPlus
import Control.MonadZero as Control.MonadZero
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data KeyboardAction = Left | Right | Up | Escape

derive instance genericKeyboardAction :: Generic.Rep.Generic KeyboardAction _
instance showKeyboardAction :: Show KeyboardAction where
  show = Show.Generic.genericShow

fromKeyboardEvent :: KeyboardEvent.KeyboardEvent -> Maybe.Maybe KeyboardAction
fromKeyboardEvent event =
  ( (Control.MonadZero.guard (KeyboardEvent.key event == "ArrowLeft") *> pure Left) Control.MonadPlus.<|>
    (Control.MonadZero.guard (KeyboardEvent.key event == "ArrowRight") *> pure Right) Control.MonadPlus.<|>
    (Control.MonadZero.guard (KeyboardEvent.key event == "ArrowUp") *> pure Up) Control.MonadPlus.<|>
    (Control.MonadZero.guard (KeyboardEvent.key event == "Esc") *> pure Escape) Control.MonadPlus.<|>
    (Control.MonadZero.guard (KeyboardEvent.key event == "Escape") *> pure Escape)
  )
