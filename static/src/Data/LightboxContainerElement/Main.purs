module Data.LightboxContainerElement.Main where

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
import Data.LightboxState.Main as LightboxState
import Data.LightboxActiveState.Main as LightboxActiveState
import Data.LightboxInactiveState.Main as LightboxInactiveState
import Data.LightboxSlideElement.Main as LightboxSlideElement
import Data.LightboxThumbnailElement.Main as LightboxThumbnailElement
import Data.LightboxThumbnailsElement.Main as LightboxThumbnailsElement
import Data.DocumentElement.Main as DocumentElement

type LightboxContainerElement = Web.DOM.Element.Element

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array LightboxContainerElement)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array LightboxContainerElement)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-component=lightbox_container]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array LightboxContainerElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

fromElements :: String -> Array Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe LightboxContainerElement)
fromElements id xs = do
  let emxs = fromElement id <$> xs
  matches <- Foldable.foldM (\acc -> map $ Maybe.maybe acc (Array.snoc acc)) [] emxs
  pure $ Array.head matches

fromElement :: String -> Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe LightboxContainerElement)
fromElement target element = MaybeT.runMaybeT do
  id <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "id" element
  Control.MonadZero.guard (target == id)
  pure element

open :: String -> LightboxContainerElement -> Web.HTML.Window.Window -> Effect.Effect Unit
open id containerElement window = do
  void $ MaybeT.runMaybeT do
    containerHtmlElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement containerElement
    do
      classList <- Trans.lift $ Web.HTML.HTMLElement.classList containerHtmlElement
      Trans.lift $ Web.DOM.DOMTokenList.remove classList "hidden"
    do
      slideElements <- Trans.lift $ LightboxSlideElement.fromHTMLElement containerHtmlElement
      Traversable.for_ slideElements $ \slideElement -> do
        slideHTMLElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement slideElement
        classList <- Trans.lift $ Web.HTML.HTMLElement.classList slideHTMLElement
        Trans.lift $ Web.DOM.DOMTokenList.remove classList "active"
    do
      slideElement <- MaybeT.MaybeT $ LightboxSlideElement.fromContainerHTMLElement id containerHtmlElement
      slideHtmlElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement slideElement
      classList <- Trans.lift $ Web.HTML.HTMLElement.classList slideHtmlElement
      Trans.lift $ Web.DOM.DOMTokenList.add classList "active"
    do
      thumbnailElements <- Trans.lift $ LightboxThumbnailElement.fromHTMLElement containerHtmlElement
      Traversable.for_ thumbnailElements $ \thumbnailElement -> do
        thumbnailHTMLElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement thumbnailElement
        classList <- Trans.lift $ Web.HTML.HTMLElement.classList thumbnailHTMLElement
        Trans.lift $ Web.DOM.DOMTokenList.remove classList "active"
    do
      thumbnailElement <- MaybeT.MaybeT $ LightboxThumbnailElement.fromContainerHTMLElement id containerHtmlElement
      thumbnailHTMLElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement thumbnailElement
      do
        classList <- Trans.lift $ Web.HTML.HTMLElement.classList thumbnailHTMLElement
        Trans.lift $ Web.DOM.DOMTokenList.add classList "active"
      do
        thumbnailsElement <- MaybeT.MaybeT $ LightboxThumbnailsElement.fromContainerHTMLElement containerHtmlElement
        Trans.lift $ LightboxThumbnailsElement.scrollTo thumbnailElement thumbnailsElement
    do
      documentElement <- MaybeT.MaybeT $ DocumentElement.fromWindow window
      Trans.lift $ DocumentElement.disableScroll documentElement

close :: LightboxContainerElement -> Web.HTML.Window.Window -> Effect.Effect Unit
close containerElement window = do
  void $ MaybeT.runMaybeT do
    containerHtmlElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement containerElement
    do
      slideElements <- Trans.lift $ LightboxSlideElement.fromHTMLElement containerHtmlElement
      Traversable.for_ slideElements $ \slideElement -> do
        slideHTMLElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement slideElement
        classList <- Trans.lift $ Web.HTML.HTMLElement.classList slideHTMLElement
        Trans.lift $ Web.DOM.DOMTokenList.remove classList "active"
    do
      thumbnailElements <- Trans.lift $ LightboxThumbnailElement.fromHTMLElement containerHtmlElement
      Traversable.for_ thumbnailElements $ \thumbnailElement -> do
        thumbnailHTMLElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement thumbnailElement
        classList <- Trans.lift $ Web.HTML.HTMLElement.classList thumbnailHTMLElement
        Trans.lift $ Web.DOM.DOMTokenList.remove classList "active"
    do
      classList <- Trans.lift $ Web.HTML.HTMLElement.classList containerHtmlElement
      Trans.lift $ Web.DOM.DOMTokenList.add classList "hidden"
    do
      documentElement <- MaybeT.MaybeT $ DocumentElement.fromWindow window
      Trans.lift $ DocumentElement.enableScroll documentElement

onState :: LightboxState.LightboxState -> LightboxContainerElement -> Web.HTML.Window.Window -> Effect.Effect Unit
onState (LightboxState.Active (LightboxActiveState.LightboxActiveState state)) window = open state.id window
onState (LightboxState.Inactive _) window = close window
