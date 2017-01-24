module Halogen.HTML.Events
  ( input
  , input_
  , handler
  , onAbort
  , onError
  , onLoad
  , onScroll
  , onChange
  , onInput
  , onInvalid
  , onReset
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

import DOM.Event.Types (Event, EventType(..), FocusEvent, KeyboardEvent, MouseEvent)
import DOM.Event.Event as EE
import DOM.HTML.Event.Types (DragEvent)

import Halogen.Query (Action, action)
import Halogen.Query.InputF (InputF(..))
import Halogen.HTML.Core (Prop)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp, IndexedProp, I)

import Unsafe.Coerce (unsafeCoerce)

input :: forall f a. (a -> Action f) -> a -> Maybe (f Unit)
input f x = Just $ action (f x)

input_ :: forall f a. Action f -> a -> Maybe (f Unit)
input_ f _ = Just $ action f

handler :: forall r f p. EventType -> (Event -> Maybe (f Unit)) -> IProp r f p
handler et = (unsafeCoerce :: forall i. (EventType -> (Event -> Maybe i) -> Prop i) -> EventType -> (Event -> Maybe i) -> IndexedProp r i) Core.handler et <<< map (map Query)

onAbort :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onAbort :: I | r) f p
onAbort = handler (EventType "abort")

onError :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onError :: I | r) f p
onError = handler (EventType "error")

onLoad :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onLoad :: I | r) f p
onLoad = handler (EventType "load")

onScroll :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onScroll :: I | r) f p
onScroll = handler (EventType "scroll")

onChange :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onChange :: I | r) f p
onChange = handler (EventType "change")

onInput :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onInput :: I | r) f p
onInput = handler (EventType "input")

onInvalid :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onInvalid :: I | r) f p
onInvalid = handler (EventType "invalid")

onReset :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onReset :: I | r) f p
onReset = handler (EventType "reset")

onSelect :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onSelect :: I | r) f p
onSelect = handler (EventType "select")

onSubmit :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onSubmit :: I | r) f p
onSubmit = handler (EventType "submit")

onTransitionEnd :: forall r f p. (Event -> Maybe (f Unit)) -> IProp (onTransitionEnd :: I | r) f p
onTransitionEnd = handler (EventType "transitionend")

onClick :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onClick :: I | r) f p
onClick = handler (EventType "click") <<< mouseHandler

onContextMenu :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onContextMenu :: I | r) f p
onContextMenu = handler (EventType "contextmenu") <<< mouseHandler

onDoubleClick :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onDoubleClick :: I | r) f p
onDoubleClick = handler (EventType "dblclick") <<< mouseHandler

onMouseDown :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseDown :: I | r) f p
onMouseDown = handler (EventType "mousedown") <<< mouseHandler

onMouseEnter :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseEnter :: I | r) f p
onMouseEnter = handler (EventType "mouseenter") <<< mouseHandler

onMouseLeave :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseLeave :: I | r) f p
onMouseLeave = handler (EventType "mouseleave") <<< mouseHandler

onMouseMove :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseMove :: I | r) f p
onMouseMove = handler (EventType "mousemove") <<< mouseHandler

onMouseOver :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseOver :: I | r) f p
onMouseOver = handler (EventType "mouseover") <<< mouseHandler

onMouseOut :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseOut :: I | r) f p
onMouseOut = handler (EventType "mouseout") <<< mouseHandler

onMouseUp :: forall r f p. (MouseEvent -> Maybe (f Unit)) -> IProp (onMouseUp :: I | r) f p
onMouseUp = handler (EventType "mouseup") <<< mouseHandler

onKeyDown :: forall r f p. (KeyboardEvent -> Maybe (f Unit)) -> IProp (onKeyDown :: I | r) f p
onKeyDown = handler (EventType "keydown") <<< keyHandler

onKeyPress :: forall r f p. (KeyboardEvent -> Maybe (f Unit)) -> IProp (onKeyPress :: I | r) f p
onKeyPress = handler (EventType "keypress") <<< keyHandler

onKeyUp :: forall r f p. (KeyboardEvent -> Maybe (f Unit)) -> IProp (onKeyUp :: I | r) f p
onKeyUp = handler (EventType "keyup") <<< keyHandler

onBlur :: forall r f p. (FocusEvent -> Maybe (f Unit)) -> IProp (onBlur :: I | r) f p
onBlur = handler (EventType "blur") <<< focusHandler

onFocus :: forall r f p. (FocusEvent -> Maybe (f Unit)) -> IProp (onFocus :: I | r) f p
onFocus = handler (EventType "focus") <<< focusHandler

onFocusIn :: forall r f p. (FocusEvent -> Maybe (f Unit)) -> IProp (onFocusIn :: I | r) f p
onFocusIn = handler (EventType "focusin") <<< focusHandler

onFocusOut :: forall r f p. (FocusEvent -> Maybe (f Unit)) -> IProp (onFocusOut :: I | r) f p
onFocusOut = handler (EventType "focusout") <<< focusHandler

onDrag :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDrag :: I | r) f p
onDrag = handler (EventType "drag") <<< dragHandler

onDragEnd :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDragEnd :: I | r) f p
onDragEnd = handler (EventType "dragend") <<< dragHandler

onDragExit :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDragExit :: I | r) f p
onDragExit = handler (EventType "dragexit") <<< dragHandler

onDragEnter :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDragEnter :: I | r) f p
onDragEnter = handler (EventType "dragenter") <<< dragHandler

onDragLeave :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDragLeave :: I | r) f p
onDragLeave = handler (EventType "dragleave") <<< dragHandler

onDragOver :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDragOver :: I | r) f p
onDragOver = handler (EventType "dragover") <<< dragHandler

onDragStart :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDragStart :: I | r) f p
onDragStart = handler (EventType "dragstart") <<< dragHandler

onDrop :: forall r f p. (DragEvent -> Maybe (f Unit)) -> IProp (onDrop :: I | r) f p
onDrop = handler (EventType "drop") <<< dragHandler

keyHandler :: forall i. (KeyboardEvent -> Maybe i) -> Event -> Maybe i
keyHandler = unsafeCoerce

mouseHandler :: forall i. (MouseEvent -> Maybe i) -> Event -> Maybe i
mouseHandler = unsafeCoerce

focusHandler :: forall i. (FocusEvent -> Maybe i) -> Event -> Maybe i
focusHandler = unsafeCoerce

dragHandler :: forall i. (DragEvent -> Maybe i) -> Event -> Maybe i
dragHandler = unsafeCoerce

-- | Attaches event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall r f p value. IsForeign value => EventType -> String -> (value -> Maybe (f Unit)) -> IProp r f p
addForeignPropHandler key prop f =
  handler key (either (const Nothing) f <<< runExcept <<< readProp prop <<< toForeign <<< EE.currentTarget)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r f p. (String -> Maybe (f Unit)) -> IProp (value :: I, onChange :: I | r) f p
onValueChange = addForeignPropHandler (EventType "change") "value"

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r f p. (Int -> Maybe (f Unit)) -> IProp (selectedIndex :: I, onChange :: I | r) f p
onSelectedIndexChange = addForeignPropHandler (EventType "change") "selectedIndex"

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r f p. (String -> Maybe (f Unit)) -> IProp (value :: I, onInput :: I | r) f p
onValueInput = addForeignPropHandler (EventType "input") "value"

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r f p. (Boolean -> Maybe (f Unit)) -> IProp (checked :: I, onChange :: I | r) f p
onChecked = addForeignPropHandler (EventType "change") "checked"
