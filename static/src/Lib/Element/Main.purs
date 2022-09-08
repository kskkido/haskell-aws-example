module Lib.Element.Main where

import Prelude
import Effect as Effect
import Web.DOM.Element as Web.DOM.Element

type DOMRect =
  { top :: Number
  , right :: Number
  , bottom :: Number
  , left :: Number
  , width :: Number
  , height :: Number
  , x :: Number
  , y :: Number
  }

foreign import getBoundingClientRect :: Web.DOM.Element.Element -> Effect.Effect DOMRect
