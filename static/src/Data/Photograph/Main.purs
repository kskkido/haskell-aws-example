module Data.Photograph.Main where

import Prelude
import Effect as Effect
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Traversable as Traversable
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument

type Photograph =
  { id :: String
  , title :: String
  , url :: String
  , fileName :: String
  , size :: Number
  , width :: Number
  , height :: Number
  }

fromWindow :: Web.HTML.Window.Window -> Effect.Effect (Array Photograph)
fromWindow window = do
  parentNode <- Web.HTML.HTMLDocument.toParentNode <$> Web.HTML.Window.document window
  fromParentNode parentNode

fromParentNode :: Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array Photograph)
fromParentNode parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector "[data-type=photograph]"
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array Photograph)
fromNodeList nodeList = do
  elements <- Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList
  Array.catMaybes <$> Traversable.traverse fromElement elements

fromElement :: Web.DOM.Element.Element -> Effect.Effect (Maybe.Maybe Photograph)
fromElement element = do
  MaybeT.runMaybeT do
    id <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-id" element
    title <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-title" element
    url <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-url" element
    fileName <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-filename" element
    size <- do
      attribute <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-size" element
      MaybeT.MaybeT $ pure $ Number.fromString attribute
    width <- do
      attribute <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-width" element
      MaybeT.MaybeT $ pure $ Number.fromString attribute
    height <- do
      attribute <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-height" element
      MaybeT.MaybeT $ pure $ Number.fromString attribute
    pure $
      { id: id
      , title: title
      , url: url
      , fileName: fileName
      , size: size
      , width: width
      , height: height
      }
