-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.

module Halogen.HTML.Events
  ( input
  , input_
  , onAbort
  , onBeforeUnload
  , onError
  , onHashChange
  , onLoad
  , onPageShow
  , onPageHide
  , onResize
  , onScroll
  , onUnload
  , onChange
  , onInvalid
  , onReset
  , onSearch
  , onSelect
  , onSubmit
  , onClick
  , onContextMenu
  , onDoubleClick
  , onMouseDown
  , onMouseEnter
  , onMouseLeave
  , onMouseMove
  , onMouseOver
  , onMouseOut
  , onMouseUp
  , onKeyDown
  , onKeyPress
  , onKeyUp
  , onBlur
  , onFocus
  , onFocusIn
  , onFocusOut
  ) where

import Data.Maybe

import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Types

import qualified Halogen.HTML.Attributes as H

-- | A helper function which can be used to create simple event handlers.
-- |
-- | Often we don't need to use `EventHandler` or the monad underlying our component, and just need
-- | to generate an input to the signal function.
-- |
-- | This function provides an alternative to making two nested calls to `pure`:
-- |
-- | ```purescript
-- | onClick (input \_ -> Input)
-- | ```
input :: forall i m a. (Applicative m) => (a -> i) -> a -> EventHandler (m i)
input f e = pure (pure (f e))

-- | A helper function for simple event handlers that provide an input to the signal function,
-- | where there is no need to make use of the event value to generate the input.
-- |
-- | ```purescript
-- | onclick (input_ Input)
-- | ```
input_ :: forall i m a. (Applicative m) => i -> a -> EventHandler (m i)
input_ x _ = pure (pure x)

onAbort	:: forall i. (Event () -> EventHandler i) -> H.Attr i
onAbort = H.handler (H.eventName "abort")

onBeforeUnload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onBeforeUnload = H.handler (H.eventName "beforeunload")

onError :: forall i. (Event () -> EventHandler i) -> H.Attr i
onError = H.handler (H.eventName "error")

onHashChange :: forall i. (Event () -> EventHandler i) -> H.Attr i
onHashChange = H.handler (H.eventName "hashchange")

onLoad :: forall i. (Event () -> EventHandler i) -> H.Attr i
onLoad = H.handler (H.eventName "load")

onPageShow :: forall i. (Event () -> EventHandler i) -> H.Attr i
onPageShow = H.handler (H.eventName "pageshow")

onPageHide :: forall i. (Event () -> EventHandler i) -> H.Attr i
onPageHide = H.handler (H.eventName "pagehide")

onResize :: forall i. (Event () -> EventHandler i) -> H.Attr i
onResize = H.handler (H.eventName "resize")

onScroll :: forall i. (Event () -> EventHandler i) -> H.Attr i
onScroll = H.handler (H.eventName "scroll")

onUnload :: forall i. (Event () -> EventHandler i) -> H.Attr i
onUnload = H.handler (H.eventName "unload")

onChange :: forall i. (Event () -> EventHandler i) -> H.Attr i
onChange = H.handler (H.eventName "change")

onInvalid :: forall i. (Event () -> EventHandler i) -> H.Attr i
onInvalid = H.handler (H.eventName "invalid")

onReset :: forall i. (Event () -> EventHandler i) -> H.Attr i
onReset = H.handler (H.eventName "reset")

onSearch :: forall i. (Event () -> EventHandler i) -> H.Attr i
onSearch = H.handler (H.eventName "search")

onSelect :: forall i. (Event () -> EventHandler i) -> H.Attr i
onSelect = H.handler (H.eventName "select")

onSubmit :: forall i. (Event () -> EventHandler i) -> H.Attr i
onSubmit = H.handler (H.eventName "submit")

onClick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onClick = H.handler (H.eventName "click")

onContextMenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onContextMenu = H.handler (H.eventName "contextmenu")

onDoubleClick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onDoubleClick = H.handler (H.eventName "dblclick")

onMouseDown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseDown = H.handler (H.eventName "mousedown")

onMouseEnter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseEnter = H.handler (H.eventName "mouseenter")

onMouseLeave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseLeave = H.handler (H.eventName "mouseleave")

onMouseMove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseMove = H.handler (H.eventName "mousemove")

onMouseOver :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseOver = H.handler (H.eventName "mouseover")

onMouseOut :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseOut = H.handler (H.eventName "mouseout")

onMouseUp :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
onMouseUp = H.handler (H.eventName "mouseup")

onKeyDown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onKeyDown = H.handler (H.eventName "keydown")

onKeyPress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onKeyPress = H.handler (H.eventName "keypress")

onKeyUp :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
onKeyUp = H.handler (H.eventName "keyup")

onBlur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onBlur = H.handler (H.eventName "blur")

onFocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onFocus = H.handler (H.eventName "focus")

onFocusIn :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onFocusIn = H.handler (H.eventName "focusin")

onFocusOut :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
onFocusOut = H.handler (H.eventName "focusout")
