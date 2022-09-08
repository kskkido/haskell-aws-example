module Data.LightboxSlideElement.Main where

import Prelude
import Effect as Effect
import Control.MonadZero as Control.MonadZero
import Control.Monad.Trans.Class as Trans
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument

type LightboxSlideElement = Web.DOM.Element.Element

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array LightboxSlideElement)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromHTMLElement :: Web.HTML.HTMLElement.HTMLElement -> Effect.Effect (Array LightboxSlideElement)
fromHTMLElement htmlElement = do
  let parentNode = Web.HTML.HTMLElement.toParentNode htmlElement
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array LightboxSlideElement)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-component=lightbox_slide]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array LightboxSlideElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

fromElements :: String -> Array Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe LightboxSlideElement)
fromElements id xs = do
  let emxs = fromElement id <$> xs
  matches <- Foldable.foldM (\acc -> map $ Maybe.maybe acc (Array.snoc acc)) [] emxs
  pure $ Array.head matches

fromElement :: String -> Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe LightboxSlideElement)
fromElement target element = MaybeT.runMaybeT do
  id <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-lightbox_slide_id" element
  Control.MonadZero.guard (target == id)
  pure element

fromContainerHTMLElement :: String -> Web.HTML.HTMLElement.HTMLElement -> Effect.Effect (Maybe.Maybe LightboxSlideElement)
fromContainerHTMLElement id element = do
  let parentNode = Web.HTML.HTMLElement.toParentNode element
  fromParentNode parentNode >>= fromElements id
