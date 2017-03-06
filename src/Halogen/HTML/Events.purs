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
import Halogen.HTML.Properties (IProp)

import Unsafe.Coerce (unsafeCoerce)

input :: forall f a. (a -> Action f) -> a -> Maybe (f Unit)
input f x = Just $ action (f x)

input_ :: forall f a. Action f -> a -> Maybe (f Unit)
input_ f _ = Just $ action f

handler :: forall r i. EventType -> (Event -> Maybe i) -> IProp r i
handler et = (unsafeCoerce :: (EventType -> (Event -> Maybe i) -> Prop i) -> EventType -> (Event -> Maybe (InputF Unit i)) -> IProp r i) Core.handler et <<< map (map Query)

onAbort :: forall r i. (Event -> Maybe i) -> IProp (onAbort :: Event | r) i
onAbort = handler (EventType "abort")

onError :: forall r i. (Event -> Maybe i) -> IProp (onError :: Event | r) i
onError = handler (EventType "error")

onLoad :: forall r i. (Event -> Maybe i) -> IProp (onLoad :: Event | r) i
onLoad = handler (EventType "load")

onScroll :: forall r i. (Event -> Maybe i) -> IProp (onScroll :: Event | r) i
onScroll = handler (EventType "scroll")

onChange :: forall r i. (Event -> Maybe i) -> IProp (onChange :: Event | r) i
onChange = handler (EventType "change")

onInput :: forall r i. (Event -> Maybe i) -> IProp (onInput :: Event | r) i
onInput = handler (EventType "input")

onInvalid :: forall r i. (Event -> Maybe i) -> IProp (onInvalid :: Event | r) i
onInvalid = handler (EventType "invalid")

onReset :: forall r i. (Event -> Maybe i) -> IProp (onReset :: Event | r) i
onReset = handler (EventType "reset")

onSelect :: forall r i. (Event -> Maybe i) -> IProp (onSelect :: Event | r) i
onSelect = handler (EventType "select")

onSubmit :: forall r i. (Event -> Maybe i) -> IProp (onSubmit :: Event | r) i
onSubmit = handler (EventType "submit")

onTransitionEnd :: forall r i. (Event -> Maybe i) -> IProp (onTransitionEnd :: Event | r) i
onTransitionEnd = handler (EventType "transitionend")

onClick :: forall r i. (MouseEvent -> Maybe i) -> IProp (onClick :: MouseEvent | r) i
onClick = handler (EventType "click") <<< mouseHandler

onContextMenu :: forall r i. (MouseEvent -> Maybe i) -> IProp (onContextMenu :: MouseEvent | r) i
onContextMenu = handler (EventType "contextmenu") <<< mouseHandler

onDoubleClick :: forall r i. (MouseEvent -> Maybe i) -> IProp (onDoubleClick :: MouseEvent | r) i
onDoubleClick = handler (EventType "dblclick") <<< mouseHandler

onMouseDown :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseDown :: MouseEvent | r) i
onMouseDown = handler (EventType "mousedown") <<< mouseHandler

onMouseEnter :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseEnter :: MouseEvent | r) i
onMouseEnter = handler (EventType "mouseenter") <<< mouseHandler

onMouseLeave :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseLeave :: MouseEvent | r) i
onMouseLeave = handler (EventType "mouseleave") <<< mouseHandler

onMouseMove :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseMove :: MouseEvent | r) i
onMouseMove = handler (EventType "mousemove") <<< mouseHandler

onMouseOver :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseOver :: MouseEvent | r) i
onMouseOver = handler (EventType "mouseover") <<< mouseHandler

onMouseOut :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseOut :: MouseEvent | r) i
onMouseOut = handler (EventType "mouseout") <<< mouseHandler

onMouseUp :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseUp :: MouseEvent | r) i
onMouseUp = handler (EventType "mouseup") <<< mouseHandler

onKeyDown :: forall r i. (KeyboardEvent -> Maybe i) -> IProp (onKeyDown :: KeyboardEvent | r) i
onKeyDown = handler (EventType "keydown") <<< keyHandler

onKeyPress :: forall r i. (KeyboardEvent -> Maybe i) -> IProp (onKeyPress :: KeyboardEvent | r) i
onKeyPress = handler (EventType "keypress") <<< keyHandler

onKeyUp :: forall r i. (KeyboardEvent -> Maybe i) -> IProp (onKeyUp :: KeyboardEvent | r) i
onKeyUp = handler (EventType "keyup") <<< keyHandler

onBlur :: forall r i. (FocusEvent -> Maybe i) -> IProp (onBlur :: FocusEvent | r) i
onBlur = handler (EventType "blur") <<< focusHandler

onFocus :: forall r i. (FocusEvent -> Maybe i) -> IProp (onFocus :: FocusEvent | r) i
onFocus = handler (EventType "focus") <<< focusHandler

onFocusIn :: forall r i. (FocusEvent -> Maybe i) -> IProp (onFocusIn :: FocusEvent | r) i
onFocusIn = handler (EventType "focusin") <<< focusHandler

onFocusOut :: forall r i. (FocusEvent -> Maybe i) -> IProp (onFocusOut :: FocusEvent | r) i
onFocusOut = handler (EventType "focusout") <<< focusHandler

onDrag :: forall r i. (DragEvent -> Maybe i) -> IProp (onDrag :: DragEvent | r) i
onDrag = handler (EventType "drag") <<< dragHandler

onDragEnd :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragEnd :: DragEvent | r) i
onDragEnd = handler (EventType "dragend") <<< dragHandler

onDragExit :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragExit :: DragEvent | r) i
onDragExit = handler (EventType "dragexit") <<< dragHandler

onDragEnter :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragEnter :: DragEvent | r) i
onDragEnter = handler (EventType "dragenter") <<< dragHandler

onDragLeave :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragLeave :: DragEvent | r) i
onDragLeave = handler (EventType "dragleave") <<< dragHandler

onDragOver :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragOver :: DragEvent | r) i
onDragOver = handler (EventType "dragover") <<< dragHandler

onDragStart :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragStart :: DragEvent | r) i
onDragStart = handler (EventType "dragstart") <<< dragHandler

onDrop :: forall r i. (DragEvent -> Maybe i) -> IProp (onDrop :: DragEvent | r) i
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
addForeignPropHandler :: forall r i value. IsForeign value => EventType -> String -> (value -> Maybe i) -> IProp r i
addForeignPropHandler key prop f =
  handler key (either (const Nothing) f <<< runExcept <<< readProp prop <<< toForeign <<< EE.currentTarget)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r i. (String -> Maybe i) -> IProp (value :: String, onChange :: Event | r) i
onValueChange = addForeignPropHandler (EventType "change") "value"

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r i. (Int -> Maybe i) -> IProp (selectedIndex :: Int, onChange :: Event | r) i
onSelectedIndexChange = addForeignPropHandler (EventType "change") "selectedIndex"

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r i. (String -> Maybe i) -> IProp (value :: String, onInput :: Event | r) i
onValueInput = addForeignPropHandler (EventType "input") "value"

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r i. (Boolean -> Maybe i) -> IProp (checked :: Boolean, onChange :: Event | r) i
onChecked = addForeignPropHandler (EventType "change") "checked"
