-- | This module defines well-typed wrappers for common DOM events, so that
-- | they may be safely embedded in HTML documents.
module Halogen.HTML.Events where

import Prelude

import Control.Monad.Free (Free())

import Data.Coyoneda (Coyoneda(), liftCoyoneda)
import Data.Inject (Inject)

import Halogen (actionF, actionFC)
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Types (Event(), MouseEvent(), FocusEvent(), KeyboardEvent())
import Halogen.HTML.Core (Prop(), handler, eventName)

inputF :: forall f g a. (Functor f, Functor g, Inject f g) => (forall i. a -> i -> f i) -> a -> EventHandler (Free g Unit)
inputF f x = pure $ actionF (f x)

inputF_ :: forall f g a. (Functor f, Functor g, Inject f g) => (forall i. i -> f i) -> a -> EventHandler (Free g Unit)
inputF_ f _ = pure $ actionF f

inputFC :: forall f g a. (Functor g, Inject (Coyoneda f) g) => (forall i. a -> i -> f i) -> a -> EventHandler (Free g Unit)
inputFC f x = pure $ actionFC (f x)

inputFC_ :: forall f g a. (Functor g, Inject (Coyoneda f) g) => (forall i. i -> f i) -> a -> EventHandler (Free g Unit)
inputFC_ f _ = pure $ actionFC f

onAbort	:: forall i. (Event () -> EventHandler i) -> Prop i
onAbort = handler (eventName "abort")

onBeforeUnload :: forall i. (Event () -> EventHandler i) -> Prop i
onBeforeUnload = handler (eventName "beforeunload")

onError :: forall i. (Event () -> EventHandler i) -> Prop i
onError = handler (eventName "error")

onHashChange :: forall i. (Event () -> EventHandler i) -> Prop i
onHashChange = handler (eventName "hashchange")

onLoad :: forall i. (Event () -> EventHandler i) -> Prop i
onLoad = handler (eventName "load")

onPageShow :: forall i. (Event () -> EventHandler i) -> Prop i
onPageShow = handler (eventName "pageshow")

onPageHide :: forall i. (Event () -> EventHandler i) -> Prop i
onPageHide = handler (eventName "pagehide")

onResize :: forall i. (Event () -> EventHandler i) -> Prop i
onResize = handler (eventName "resize")

onScroll :: forall i. (Event () -> EventHandler i) -> Prop i
onScroll = handler (eventName "scroll")

onUnload :: forall i. (Event () -> EventHandler i) -> Prop i
onUnload = handler (eventName "unload")

onChange :: forall i. (Event () -> EventHandler i) -> Prop i
onChange = handler (eventName "change")

onInput :: forall i. (Event () -> EventHandler i) -> Prop i
onInput = handler (eventName "input")

onInvalid :: forall i. (Event () -> EventHandler i) -> Prop i
onInvalid = handler (eventName "invalid")

onReset :: forall i. (Event () -> EventHandler i) -> Prop i
onReset = handler (eventName "reset")

onSearch :: forall i. (Event () -> EventHandler i) -> Prop i
onSearch = handler (eventName "search")

onSelect :: forall i. (Event () -> EventHandler i) -> Prop i
onSelect = handler (eventName "select")

onSubmit :: forall i. (Event () -> EventHandler i) -> Prop i
onSubmit = handler (eventName "submit")

onClick :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onClick = handler (eventName "click")

onContextMenu :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onContextMenu = handler (eventName "contextmenu")

onDoubleClick :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onDoubleClick = handler (eventName "dblclick")

onMouseDown :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseDown = handler (eventName "mousedown")

onMouseEnter :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseEnter = handler (eventName "mouseenter")

onMouseLeave :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseLeave = handler (eventName "mouseleave")

onMouseMove :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseMove = handler (eventName "mousemove")

onMouseOver :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseOver = handler (eventName "mouseover")

onMouseOut :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseOut = handler (eventName "mouseout")

onMouseUp :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
onMouseUp = handler (eventName "mouseup")

onKeyDown :: forall i. (Event KeyboardEvent -> EventHandler i) -> Prop i
onKeyDown = handler (eventName "keydown")

onKeyPress :: forall i. (Event KeyboardEvent -> EventHandler i) -> Prop i
onKeyPress = handler (eventName "keypress")

onKeyUp :: forall i. (Event KeyboardEvent -> EventHandler i) -> Prop i
onKeyUp = handler (eventName "keyup")

onBlur :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
onBlur = handler (eventName "blur")

onFocus :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
onFocus = handler (eventName "focus")

onFocusIn :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
onFocusIn = handler (eventName "focusin")

onFocusOut :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
onFocusOut = handler (eventName "focusout")
