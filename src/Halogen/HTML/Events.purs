module Halogen.HTML.Events
  ( input
  , input_
  , IEventProp
  , handler
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
  , onInput
  , onInvalid
  , onReset
  , onSearch
  , onSelect
  , onSubmit
  , onTransitionEnd
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
  , onDrag
  , onDragEnd
  , onDragExit
  , onDragEnter
  , onDragLeave
  , onDragOver
  , onDragStart
  , onDrop
  , onValueChange
  , onValueInput
  , onSelectedIndexChange
  , onChecked
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Maybe (Maybe(..))

import Halogen.Query (Action, action)
import Halogen.HTML.Core (EventName(..), Prop)
import Halogen.HTML.Core as Core
import Halogen.HTML.Events.Handler (EventHandler)
import Halogen.HTML.Events.Types (Event, MouseEvent, DragEvent, FocusEvent, KeyboardEvent)
import Halogen.HTML.Properties (IProp, I)

import Unsafe.Coerce (unsafeCoerce)

input :: forall f a. (a -> Action f) -> a -> EventHandler (Maybe (f Unit))
input f x = pure $ Just $ action (f x)

input_ :: forall f a. Action f -> a -> EventHandler (Maybe (f Unit))
input_ f _ = pure $ Just $ action f

type IEventProp r e i = (Event e -> EventHandler (Maybe i)) -> IProp r i

handler :: forall r e i. EventName e -> IEventProp r e i
handler = (unsafeCoerce :: (EventName e -> (Event e -> EventHandler (Maybe i)) -> Prop i) -> EventName e -> IEventProp r e i) Core.handler

onAbort :: forall r i. IEventProp (onAbort :: I | r) () i
onAbort = handler (EventName "abort")

onBeforeUnload :: forall r i. IEventProp (onBeforeUnload :: I | r) () i
onBeforeUnload = handler (EventName "beforeunload")

onError :: forall r i. IEventProp (onError :: I | r) () i
onError = handler (EventName "error")

onHashChange :: forall r i. IEventProp (onHashChange :: I | r) () i
onHashChange = handler (EventName "hashchange")

onLoad :: forall r i. IEventProp (onLoad :: I | r) () i
onLoad = handler (EventName "load")

onPageShow :: forall r i. IEventProp (onPageShow :: I | r) () i
onPageShow = handler (EventName "pageshow")

onPageHide :: forall r i. IEventProp (onPageHide :: I | r) () i
onPageHide = handler (EventName "pagehide")

onResize :: forall r i. IEventProp (onResize :: I | r) () i
onResize = handler (EventName "resize")

onScroll :: forall r i. IEventProp (onScroll :: I | r) () i
onScroll = handler (EventName "scroll")

onUnload :: forall r i. IEventProp (onUnload :: I | r) () i
onUnload = handler (EventName "unload")

onChange :: forall r i. IEventProp (onChange :: I | r) () i
onChange = handler (EventName "change")

onInput :: forall r i. IEventProp (onInput :: I | r) () i
onInput = handler (EventName "input")

onInvalid :: forall r i. IEventProp (onInvalid :: I | r) () i
onInvalid = handler (EventName "invalid")

onReset :: forall r i. IEventProp (onReset :: I | r) () i
onReset = handler (EventName "reset")

onSearch :: forall r i. IEventProp (onSearch :: I | r) () i
onSearch = handler (EventName "search")

onSelect :: forall r i. IEventProp (onSelect :: I | r) () i
onSelect = handler (EventName "select")

onSubmit :: forall r i. IEventProp (onSubmit :: I | r) () i
onSubmit = handler (EventName "submit")

onTransitionEnd :: forall r i. IEventProp (onTransitionEnd :: I | r) () i
onTransitionEnd = handler (EventName "transitionend")

onClick :: forall r i. IEventProp (onClick :: I | r) MouseEvent i
onClick = handler (EventName "click")

onContextMenu :: forall r i. IEventProp (onContextMenu :: I | r) MouseEvent i
onContextMenu = handler (EventName "contextmenu")

onDoubleClick :: forall r i. IEventProp (onDoubleClick :: I | r) MouseEvent i
onDoubleClick = handler (EventName "dblclick")

onMouseDown :: forall r i. IEventProp (onMouseDown :: I | r) MouseEvent i
onMouseDown = handler (EventName "mousedown")

onMouseEnter :: forall r i. IEventProp (onMouseEnter :: I | r) MouseEvent i
onMouseEnter = handler (EventName "mouseenter")

onMouseLeave :: forall r i. IEventProp (onMouseLeave :: I | r) MouseEvent i
onMouseLeave = handler (EventName "mouseleave")

onMouseMove :: forall r i. IEventProp (onMouseMove :: I | r) MouseEvent i
onMouseMove = handler (EventName "mousemove")

onMouseOver :: forall r i. IEventProp (onMouseOver :: I | r) MouseEvent i
onMouseOver = handler (EventName "mouseover")

onMouseOut :: forall r i. IEventProp (onMouseOut :: I | r) MouseEvent i
onMouseOut = handler (EventName "mouseout")

onMouseUp :: forall r i. IEventProp (onMouseUp :: I | r) MouseEvent i
onMouseUp = handler (EventName "mouseup")

onKeyDown :: forall r i. IEventProp (onKeyDown :: I | r) KeyboardEvent i
onKeyDown = handler (EventName "keydown")

onKeyPress :: forall r i. IEventProp (onKeyPress :: I | r) KeyboardEvent i
onKeyPress = handler (EventName "keypress")

onKeyUp :: forall r i. IEventProp (onKeyUp :: I | r) KeyboardEvent i
onKeyUp = handler (EventName "keyup")

onBlur :: forall r i. IEventProp (onBlur :: I | r) FocusEvent i
onBlur = handler (EventName "blur")

onFocus :: forall r i. IEventProp (onFocus :: I | r) FocusEvent i
onFocus = handler (EventName "focus")

onFocusIn :: forall r i. IEventProp (onFocusIn :: I | r) FocusEvent i
onFocusIn = handler (EventName "focusin")

onFocusOut :: forall r i. IEventProp (onFocusOut :: I | r) FocusEvent i
onFocusOut = handler (EventName "focusout")

onDrag :: forall r i. IEventProp r DragEvent i
onDrag = handler (EventName "drag")

onDragEnd :: forall r i. IEventProp r DragEvent i
onDragEnd = handler (EventName "dragend")

onDragExit :: forall r i. IEventProp r DragEvent i
onDragExit = handler (EventName "dragexit")

onDragEnter :: forall r i. IEventProp r DragEvent i
onDragEnter = handler (EventName "dragenter")

onDragLeave :: forall r i. IEventProp r DragEvent i
onDragLeave = handler (EventName "dragleave")

onDragOver :: forall r i. IEventProp r DragEvent i
onDragOver = handler (EventName "dragover")

onDragStart :: forall r i. IEventProp r DragEvent i
onDragStart = handler (EventName "dragstart")

onDrop :: forall r i. IEventProp r DragEvent i
onDrop = handler (EventName "drop")

-- | Attaches event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall r i value. IsForeign value => String -> String -> (value -> EventHandler (Maybe i)) -> IProp r i
addForeignPropHandler key prop f =
  handler (EventName key) (either (const $ pure Nothing) f <<< runExcept <<< readProp prop <<< toForeign <<< _.target)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r i. (String -> EventHandler (Maybe i)) -> IProp (value :: I, onChange :: I | r) i
onValueChange = addForeignPropHandler "change" "value"

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r i. (Int -> EventHandler (Maybe i)) -> IProp (selectedIndex :: I, onChange :: I | r) i
onSelectedIndexChange = addForeignPropHandler "change" "selectedIndex"

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r i. (String -> EventHandler (Maybe i)) -> IProp (value :: I, onInput :: I | r) i
onValueInput = addForeignPropHandler "input" "value"

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r i. (Boolean -> EventHandler (Maybe i)) -> IProp (checked :: I, onChange :: I | r) i
onChecked = addForeignPropHandler "change" "checked"
