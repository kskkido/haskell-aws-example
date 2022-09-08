module Data.PhotographElement.Main where

import Prelude
import Effect as Effect
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Int as Int
import Data.Traversable as Traversable
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument

type PhotographElement = Web.DOM.Element.Element

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array PhotographElement)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array PhotographElement)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-type=photograph]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array PhotographElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

