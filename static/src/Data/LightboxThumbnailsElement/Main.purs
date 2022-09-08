module Data.LightboxThumbnailsElement.Main where

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
import Lib.Element.Main as Lib.Element

type LightboxThumbnailsElement = Web.DOM.Element.Element

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array LightboxThumbnailsElement)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromHTMLElement :: Web.HTML.HTMLElement.HTMLElement -> Effect.Effect (Array LightboxThumbnailsElement)
fromHTMLElement htmlElement = do
  let parentNode = Web.HTML.HTMLElement.toParentNode htmlElement
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array LightboxThumbnailsElement)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-component=lightbox_thumbnails]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array LightboxThumbnailsElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

fromContainerHTMLElement :: Web.HTML.HTMLElement.HTMLElement -> Effect.Effect (Maybe.Maybe LightboxThumbnailsElement)
fromContainerHTMLElement element = do
  let parentNode = Web.HTML.HTMLElement.toParentNode element
  Array.head <$> fromParentNode parentNode

scrollTo :: Web.DOM.Element.Element -> LightboxThumbnailsElement -> Effect.Effect Unit
scrollTo element thumbnailsElement = do
  thumbnailsDomRect <- Lib.Element.getBoundingClientRect thumbnailsElement
  elementDomRect <- Lib.Element.getBoundingClientRect element
  void $ MaybeT.runMaybeT do
    htmlElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement element
    elementOffsetLeft <- Trans.lift $ Web.HTML.HTMLElement.offsetLeft htmlElement
    let offset = elementOffsetLeft + (elementDomRect.width / 2.0) - (thumbnailsDomRect.width / 2.0)
    Trans.lift $ Web.DOM.Element.setScrollLeft offset thumbnailsElement
