-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.
module Halogen.HTML.Events
  ( module Halogen.HTML.Events
  , module Halogen.HTML.Events.Handler
  , module Halogen.HTML.Events.Types
  ) where

import Prelude

import Control.Monad.Free (Free())

import Halogen.Query (action)
import Halogen.HTML.Events.Handler (EventHandler(), preventDefault, stopPropagation, stopImmediatePropagation)
import Halogen.HTML.Events.Types (Event(), MouseEvent(), FocusEvent(), KeyboardEvent())
import Halogen.HTML.Core (Prop(), handler, eventName)

type EventProp e i = (Event e -> EventHandler i) -> Prop i

input :: forall f a. (forall i. a -> i -> f i) -> a -> EventHandler (f Unit)
input f x = pure $ action (f x)

input_ :: forall f a. (forall i. i -> f i) -> a -> EventHandler (f Unit)
input_ f _ = pure $ action f

onAbort :: forall i. EventProp () i
onAbort = handler (eventName "abort")

onBeforeUnload :: forall i. EventProp () i
onBeforeUnload = handler (eventName "beforeunload")

onError :: forall i. EventProp () i
onError = handler (eventName "error")

onHashChange :: forall i. EventProp () i
onHashChange = handler (eventName "hashchange")

onLoad :: forall i. EventProp () i
onLoad = handler (eventName "load")

onPageShow :: forall i. EventProp () i
onPageShow = handler (eventName "pageshow")

onPageHide :: forall i. EventProp () i
onPageHide = handler (eventName "pagehide")

onResize :: forall i. EventProp () i
onResize = handler (eventName "resize")

onScroll :: forall i. EventProp () i
onScroll = handler (eventName "scroll")

onUnload :: forall i. EventProp () i
onUnload = handler (eventName "unload")

onChange :: forall i. EventProp () i
onChange = handler (eventName "change")

onInput :: forall i. EventProp () i
onInput = handler (eventName "input")

onInvalid :: forall i. EventProp () i
onInvalid = handler (eventName "invalid")

onReset :: forall i. EventProp () i
onReset = handler (eventName "reset")

onSearch :: forall i. EventProp () i
onSearch = handler (eventName "search")

onSelect :: forall i. EventProp () i
onSelect = handler (eventName "select")

onSubmit :: forall i. EventProp () i
onSubmit = handler (eventName "submit")

onClick :: forall i. EventProp MouseEvent i
onClick = handler (eventName "click")

onContextMenu :: forall i. EventProp MouseEvent i
onContextMenu = handler (eventName "contextmenu")

onDoubleClick :: forall i. EventProp MouseEvent i
onDoubleClick = handler (eventName "dblclick")

onMouseDown :: forall i. EventProp MouseEvent i
onMouseDown = handler (eventName "mousedown")

onMouseEnter :: forall i. EventProp MouseEvent i
onMouseEnter = handler (eventName "mouseenter")

onMouseLeave :: forall i. EventProp MouseEvent i
onMouseLeave = handler (eventName "mouseleave")

onMouseMove :: forall i. EventProp MouseEvent i
onMouseMove = handler (eventName "mousemove")

onMouseOver :: forall i. EventProp MouseEvent i
onMouseOver = handler (eventName "mouseover")

onMouseOut :: forall i. EventProp MouseEvent i
onMouseOut = handler (eventName "mouseout")

onMouseUp :: forall i. EventProp MouseEvent i
onMouseUp = handler (eventName "mouseup")

onKeyDown :: forall i. EventProp KeyboardEvent i
onKeyDown = handler (eventName "keydown")

onKeyPress :: forall i. EventProp KeyboardEvent i
onKeyPress = handler (eventName "keypress")

onKeyUp :: forall i. EventProp KeyboardEvent i
onKeyUp = handler (eventName "keyup")

onBlur :: forall i. EventProp FocusEvent i
onBlur = handler (eventName "blur")

onFocus :: forall i. EventProp FocusEvent i
onFocus = handler (eventName "focus")

onFocusIn :: forall i. EventProp FocusEvent i
onFocusIn = handler (eventName "focusin")

onFocusOut :: forall i. EventProp FocusEvent i
onFocusOut = handler (eventName "focusout")
