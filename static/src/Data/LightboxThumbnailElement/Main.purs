module Data.LightboxThumbnailElement.Main where

import Prelude
import Effect as Effect
import Control.MonadZero as Control.MonadZero
import Control.Monad.Trans.Class as Trans
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument

type LightboxThumbnailElement = Web.DOM.Element.Element

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array LightboxThumbnailElement)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromHTMLElement :: Web.HTML.HTMLElement.HTMLElement -> Effect.Effect (Array LightboxThumbnailElement)
fromHTMLElement htmlElement = do
  let parentNode = Web.HTML.HTMLElement.toParentNode htmlElement
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array LightboxThumbnailElement)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-component=lightbox_thumbnail]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array LightboxThumbnailElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

fromElements :: String -> Array Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe LightboxThumbnailElement)
fromElements id xs = do
  let emxs = fromElement id <$> xs
  matches <- Foldable.foldM (\acc -> map $ Maybe.maybe acc (Array.snoc acc)) [] emxs
  pure $ Array.head matches

fromElement :: String -> Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe LightboxThumbnailElement)
fromElement target element = MaybeT.runMaybeT do
  id <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-lightbox_thumbnail_id" element
  Control.MonadZero.guard (target == id)
  pure element

fromContainerHTMLElement :: String -> Web.HTML.HTMLElement.HTMLElement -> Effect.Effect (Maybe.Maybe LightboxThumbnailElement)
fromContainerHTMLElement id element = do
  let parentNode = Web.HTML.HTMLElement.toParentNode element
  fromParentNode parentNode >>= fromElements id

toThumbnailId :: LightboxThumbnailElement -> Effect.Effect (Maybe.Maybe String)
toThumbnailId lightboxThumbnailElement = do
  Web.DOM.Element.getAttribute "data-lightbox_thumbnail_id" lightboxThumbnailElement

