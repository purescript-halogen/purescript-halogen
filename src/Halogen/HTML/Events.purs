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
  , onCopy
  , onPaste
  , onCut
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
import Data.Foreign (Foreign, F, toForeign, readString, readInt, readBoolean)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..))

import DOM.Event.Types (ClipboardEvent, Event, EventType(..), FocusEvent, KeyboardEvent, MouseEvent)
import DOM.Event.Event as EE
import DOM.HTML.Event.Types (DragEvent)

import Halogen.HTML.Core as Core
import Halogen.HTML.Core (Prop)
import Halogen.HTML.Properties (IProp)
import Halogen.Query (Action, action)
import Halogen.Query.InputF (InputF(..))

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

onCopy :: forall r i. (ClipboardEvent -> Maybe i) -> IProp (onCopy :: ClipboardEvent | r) i
onCopy = handler (EventType "copy") <<< clipboardHandler

onPaste :: forall r i. (ClipboardEvent -> Maybe i) -> IProp (onPaste :: ClipboardEvent | r) i
onPaste = handler (EventType "paste") <<< clipboardHandler

onCut :: forall r i. (ClipboardEvent -> Maybe i) -> IProp (onCut :: ClipboardEvent | r) i
onCut = handler (EventType "cut") <<< clipboardHandler

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

clipboardHandler :: forall i. (ClipboardEvent -> Maybe i) -> Event -> Maybe i
clipboardHandler = unsafeCoerce

-- | Attaches event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall r i value. EventType -> String -> (Foreign -> F value) -> (value -> Maybe i) -> IProp r i
addForeignPropHandler key prop reader f =
  handler key (either (const Nothing) f <<< runExcept <<< (reader <=< readProp prop) <<< toForeign <<< EE.currentTarget)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r i. (String -> Maybe i) -> IProp (value :: String, onChange :: Event | r) i
onValueChange = addForeignPropHandler (EventType "change") "value" readString

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r i. (Int -> Maybe i) -> IProp (selectedIndex :: Int, onChange :: Event | r) i
onSelectedIndexChange = addForeignPropHandler (EventType "change") "selectedIndex" readInt

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r i. (String -> Maybe i) -> IProp (value :: String, onInput :: Event | r) i
onValueInput = addForeignPropHandler (EventType "input") "value" readString

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r i. (Boolean -> Maybe i) -> IProp (checked :: Boolean, onChange :: Event | r) i
onChecked = addForeignPropHandler (EventType "change") "checked" readBoolean
