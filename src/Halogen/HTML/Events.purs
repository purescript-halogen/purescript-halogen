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
import Halogen.HTML.Properties (I, IProp)

import Unsafe.Coerce (unsafeCoerce)

input :: forall f a. (a -> Action f) -> a -> Maybe (f Unit)
input f x = Just $ action (f x)

input_ :: forall f a. Action f -> a -> Maybe (f Unit)
input_ f _ = Just $ action f

handler :: forall r p i. EventType -> (Event -> Maybe i) -> IProp r p i
handler et = (unsafeCoerce :: (EventType -> (Event -> Maybe i) -> Prop i) -> EventType -> (Event -> Maybe (InputF p Unit i)) -> IProp r p i) Core.handler et <<< map (map Query)

onAbort :: forall r p i. (Event -> Maybe i) -> IProp (onAbort :: I | r) p i
onAbort = handler (EventType "abort")

onError :: forall r p i. (Event -> Maybe i) -> IProp (onError :: I | r) p i
onError = handler (EventType "error")

onLoad :: forall r p i. (Event -> Maybe i) -> IProp (onLoad :: I | r) p i
onLoad = handler (EventType "load")

onScroll :: forall r p i. (Event -> Maybe i) -> IProp (onScroll :: I | r) p i
onScroll = handler (EventType "scroll")

onChange :: forall r p i. (Event -> Maybe i) -> IProp (onChange :: I | r) p i
onChange = handler (EventType "change")

onInput :: forall r p i. (Event -> Maybe i) -> IProp (onInput :: I | r) p i
onInput = handler (EventType "input")

onInvalid :: forall r p i. (Event -> Maybe i) -> IProp (onInvalid :: I | r) p i
onInvalid = handler (EventType "invalid")

onReset :: forall r p i. (Event -> Maybe i) -> IProp (onReset :: I | r) p i
onReset = handler (EventType "reset")

onSelect :: forall r p i. (Event -> Maybe i) -> IProp (onSelect :: I | r) p i
onSelect = handler (EventType "select")

onSubmit :: forall r p i. (Event -> Maybe i) -> IProp (onSubmit :: I | r) p i
onSubmit = handler (EventType "submit")

onTransitionEnd :: forall r p i. (Event -> Maybe i) -> IProp (onTransitionEnd :: I | r) p i
onTransitionEnd = handler (EventType "transitionend")

onClick :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onClick :: I | r) p i
onClick = handler (EventType "click") <<< mouseHandler

onContextMenu :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onContextMenu :: I | r) p i
onContextMenu = handler (EventType "contextmenu") <<< mouseHandler

onDoubleClick :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onDoubleClick :: I | r) p i
onDoubleClick = handler (EventType "dblclick") <<< mouseHandler

onMouseDown :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseDown :: I | r) p i
onMouseDown = handler (EventType "mousedown") <<< mouseHandler

onMouseEnter :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseEnter :: I | r) p i
onMouseEnter = handler (EventType "mouseenter") <<< mouseHandler

onMouseLeave :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseLeave :: I | r) p i
onMouseLeave = handler (EventType "mouseleave") <<< mouseHandler

onMouseMove :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseMove :: I | r) p i
onMouseMove = handler (EventType "mousemove") <<< mouseHandler

onMouseOver :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseOver :: I | r) p i
onMouseOver = handler (EventType "mouseover") <<< mouseHandler

onMouseOut :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseOut :: I | r) p i
onMouseOut = handler (EventType "mouseout") <<< mouseHandler

onMouseUp :: forall r p i. (MouseEvent -> Maybe i) -> IProp (onMouseUp :: I | r) p i
onMouseUp = handler (EventType "mouseup") <<< mouseHandler

onKeyDown :: forall r p i. (KeyboardEvent -> Maybe i) -> IProp (onKeyDown :: I | r) p i
onKeyDown = handler (EventType "keydown") <<< keyHandler

onKeyPress :: forall r p i. (KeyboardEvent -> Maybe i) -> IProp (onKeyPress :: I | r) p i
onKeyPress = handler (EventType "keypress") <<< keyHandler

onKeyUp :: forall r p i. (KeyboardEvent -> Maybe i) -> IProp (onKeyUp :: I | r) p i
onKeyUp = handler (EventType "keyup") <<< keyHandler

onBlur :: forall r p i. (FocusEvent -> Maybe i) -> IProp (onBlur :: I | r) p i
onBlur = handler (EventType "blur") <<< focusHandler

onFocus :: forall r p i. (FocusEvent -> Maybe i) -> IProp (onFocus :: I | r) p i
onFocus = handler (EventType "focus") <<< focusHandler

onFocusIn :: forall r p i. (FocusEvent -> Maybe i) -> IProp (onFocusIn :: I | r) p i
onFocusIn = handler (EventType "focusin") <<< focusHandler

onFocusOut :: forall r p i. (FocusEvent -> Maybe i) -> IProp (onFocusOut :: I | r) p i
onFocusOut = handler (EventType "focusout") <<< focusHandler

onDrag :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDrag :: I | r) p i
onDrag = handler (EventType "drag") <<< dragHandler

onDragEnd :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDragEnd :: I | r) p i
onDragEnd = handler (EventType "dragend") <<< dragHandler

onDragExit :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDragExit :: I | r) p i
onDragExit = handler (EventType "dragexit") <<< dragHandler

onDragEnter :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDragEnter :: I | r) p i
onDragEnter = handler (EventType "dragenter") <<< dragHandler

onDragLeave :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDragLeave :: I | r) p i
onDragLeave = handler (EventType "dragleave") <<< dragHandler

onDragOver :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDragOver :: I | r) p i
onDragOver = handler (EventType "dragover") <<< dragHandler

onDragStart :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDragStart :: I | r) p i
onDragStart = handler (EventType "dragstart") <<< dragHandler

onDrop :: forall r p i. (DragEvent -> Maybe i) -> IProp (onDrop :: I | r) p i
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
addForeignPropHandler :: forall r p i value. IsForeign value => EventType -> String -> (value -> Maybe i) -> IProp r p i
addForeignPropHandler key prop f =
  handler key (either (const Nothing) f <<< runExcept <<< readProp prop <<< toForeign <<< EE.currentTarget)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r p i. (String -> Maybe i) -> IProp (value :: I, onChange :: I | r) p i
onValueChange = addForeignPropHandler (EventType "change") "value"

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r p i. (Int -> Maybe i) -> IProp (selectedIndex :: I, onChange :: I | r) p i
onSelectedIndexChange = addForeignPropHandler (EventType "change") "selectedIndex"

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r p i. (String -> Maybe i) -> IProp (value :: I, onInput :: I | r) p i
onValueInput = addForeignPropHandler (EventType "input") "value"

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r p i. (Boolean -> Maybe i) -> IProp (checked :: I, onChange :: I | r) p i
onChecked = addForeignPropHandler (EventType "change") "checked"
