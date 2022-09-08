module Data.LightboxOverlayElement.Main where

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
import Data.LightboxContainerElement.Main as LightboxContainerElement

type LightboxOverlayElement = Web.DOM.Element.Element

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array LightboxOverlayElement)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array LightboxOverlayElement)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-component=lightbox_backdrop]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array LightboxOverlayElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

fromContainerElement :: LightboxContainerElement.LightboxContainerElement -> Effect.Effect (Maybe.Maybe LightboxOverlayElement)
fromContainerElement element = MaybeT.runMaybeT $ do
  parent <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.toParentNode <$> Web.HTML.HTMLElement.fromElement element
  matches <- Trans.lift $ fromParentNode parent
  MaybeT.MaybeT $ pure $ Array.head matches
