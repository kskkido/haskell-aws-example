module Pages.Photographs.Main where

import Prelude
import Effect as Effect
import Effect.Console as Effect.Console
import Effect.Class as Effect.Class
import Control.Monad.Cont.Trans as ContT
import Control.Monad.Maybe.Trans as MaybeT
import Control.Monad.Trans.Class as Trans
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Traversable as Traversable
import Web.Event.Event as Web.Event.Event
import Web.DOM.Element as Web.DOM.Element
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Lib.Observable.Main as Observable
import Lib.Subscription.Main as Subscription
import Lib.KeyboardAction.Main as KeyboardAction
import Data.Photograph.Main as Photograph
import Data.PhotographElement.Main as PhotographElement
import Data.LightboxContainerElement.Main as LightboxContainerElement
import Data.LightboxThumbnailElement.Main as LightboxThumbnailElement
import Data.LightboxOverlayElement.Main as LightboxOverlayElement
import Data.LightboxCloseButtonElement.Main as LightboxCloseButtonElement
import Data.LightboxEvent.Main as LightboxEvent
import Data.LightboxState.Main as LightboxState

main :: Effect.Effect Unit
main = do
  window <- Web.HTML.window
  void $ MaybeT.runMaybeT do
    lightboxContainerElement <- do
      elements <- Trans.lift $ LightboxContainerElement.fromWindow window
      MaybeT.MaybeT $ LightboxContainerElement.fromElements "lightbox_photographs" elements
    lightboxOverlayElement <- do
      MaybeT.MaybeT $ LightboxOverlayElement.fromContainerElement lightboxContainerElement
    lightboxCloseButtonElements <- do
      Trans.lift $ LightboxCloseButtonElement.fromContainerElement lightboxContainerElement
    lightboxThumbnailElements <- do
      htmlElement <- MaybeT.MaybeT $ pure $ Web.HTML.HTMLElement.fromElement lightboxContainerElement
      Trans.lift $ LightboxThumbnailElement.fromHTMLElement htmlElement
    photographElements <- Trans.lift $ do
      PhotographElement.fromWindow window
    photographs <- Trans.lift $ do
      pxs <- Traversable.traverse Photograph.fromElement photographElements
      pure $ Array.catMaybes pxs
    lightboxStateObservable <- Trans.lift $ pure do
      let ids = photographs <#> \photograph -> photograph.id
      lightboxState <-
        ( ( Observable.concat $
          ( ( photographElements <#> \element -> do
                let eventTarget = Web.DOM.Element.toEventTarget element
                    event = Web.Event.Event.EventType "click"
                callback <-
                  ( (Observable.fromEvent event eventTarget) #
                    (Observable.filterMap Web.Event.Event.target) #
                    (Observable.filterMap Web.DOM.Element.fromEventTarget) #
                    (Observable.bindIO Photograph.fromElement) #
                    (Observable.compact) #
                    (map $ \photograph -> LightboxEvent.Open photograph.id ids)
                  )
                pure callback
            ) <>
            ( lightboxThumbnailElements <#> \element -> do
                let eventTarget = Web.DOM.Element.toEventTarget element
                    event = Web.Event.Event.EventType "click"
                callback <-
                  ( (Observable.fromEvent event eventTarget) #
                    (Observable.bindIO $ const $ LightboxThumbnailElement.toThumbnailId element) #
                    (Observable.compact) #
                    (map \id -> LightboxEvent.Open id ids)
                  )
                pure callback
            ) <>
            ( lightboxCloseButtonElements <#> \element -> do
                let eventTarget = Web.DOM.Element.toEventTarget element
                    event = Web.Event.Event.EventType "click"
                callback <-
                  ( (Observable.fromEvent event eventTarget) #
                    (map $ const $ LightboxEvent.Close)
                  )
                pure callback
            ) <>
            ( [lightboxOverlayElement] <#> \element -> do
                let eventTarget = Web.DOM.Element.toEventTarget element
                    event = Web.Event.Event.EventType "click"
                callback <-
                  ( (Observable.fromEvent event eventTarget) #
                    (map $ const $ LightboxEvent.Close)
                  )
                pure callback
            )
          )
        ) #
        ( Observable.fold LightboxState.foldEvent LightboxState.unit ) #
        ( Observable.expand $ \state ->
          ( ( Observable.concat
              ( [window] <#> \w -> do
                  let eventTarget = Web.HTML.Window.toEventTarget w
                      event = Web.Event.Event.EventType "keydown"
                  callback <-
                    ( ( Observable.fromEvent event eventTarget ) #
                      ( Observable.when (LightboxState.active state ) ) #
                      ( Observable.tapIO Web.Event.Event.preventDefault ) #
                      ( Observable.filterMap Web.UIEvent.KeyboardEvent.fromEvent ) #
                      ( Observable.filterMap KeyboardAction.fromKeyboardEvent ) #
                      ( Observable.filterMap LightboxEvent.fromKeyboardAction )
                    )
                  pure callback
              )
            ) #
            ( map $ flip LightboxState.foldEvent state )
          )
        )
      )
      pure lightboxState
    Trans.lift $ (Observable.subscribe lightboxStateObservable) $ \lightboxState -> do
      LightboxContainerElement.onState lightboxState lightboxContainerElement window

