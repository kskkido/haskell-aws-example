module Data.DocumentElement.Main where

import Prelude
import Effect as Effect
import Data.Maybe as Maybe
import Web.DOM.Element as Element
import Web.DOM.DOMTokenList as DOMTokenList
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

type DocumentElement = HTMLElement.HTMLElement

fromWindow :: Window.Window -> Effect.Effect (Maybe.Maybe DocumentElement)
fromWindow window = Window.document window >>= fromHtmlDocument

fromHtmlDocument :: HTMLDocument.HTMLDocument -> Effect.Effect (Maybe.Maybe DocumentElement)
fromHtmlDocument = HTMLDocument.body

enableScroll :: DocumentElement -> Effect.Effect Unit
enableScroll documentElement = do
  let element = HTMLElement.toElement documentElement
  classList <- Element.classList element
  DOMTokenList.remove classList "no-scroll"

disableScroll :: DocumentElement -> Effect.Effect Unit
disableScroll documentElement = do
  let element = HTMLElement.toElement documentElement
  classList <- Element.classList element
  DOMTokenList.add classList "no-scroll"
