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
  -- , onContextMenu
  , onDoubleClick
  , onMouseDown
  , onMouseEnter
  , onMouseLeave
  , onMouseMove
  , onMouseOver
  , onMouseOut
  , onMouseUp
  , onWheel
  , onKeyDown
  -- , onKeyPress
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
  , onTouchCancel
  , onTouchEnd
  , onTouchEnter
  , onTouchLeave
  , onTouchMove
  , onTouchStart
  , onResize
  , onValueChange
  , onValueInput
  , onSelectedIndexChange
  , onChecked
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Foreign (F, Foreign, readBoolean, readInt, readString, unsafeToForeign)
import Foreign.Index (readProp)
import Halogen.HTML.Core (Prop)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp)
import Halogen.Query (Action, action)
import Halogen.Query.InputF (InputF(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Clipboard.ClipboardEvent.EventTypes as CET
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as EE
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent.EventTypes as DET
import Web.HTML.Event.EventTypes as ET
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.FocusEvent.EventTypes as FET
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent.EventTypes as KET
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent.EventTypes as MET
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent.EventTypes as WET

input :: forall f a. (a -> Action f) -> a -> Maybe (f Unit)
input f x = Just $ action (f x)

input_ :: forall f a. Action f -> a -> Maybe (f Unit)
input_ f _ = Just $ action f

handler :: forall r i. EventType -> (Event -> Maybe i) -> IProp r i
handler et = (unsafeCoerce :: (EventType -> (Event -> Maybe i) -> Prop i) -> EventType -> (Event -> Maybe (InputF Unit i)) -> IProp r i) Core.handler et <<< map (map Query)

onAbort :: forall r i. (Event -> Maybe i) -> IProp (onAbort :: Event | r) i
onAbort = handler (EventType "abort")

onError :: forall r i. (Event -> Maybe i) -> IProp (onError :: Event | r) i
onError = handler ET.error

onLoad :: forall r i. (Event -> Maybe i) -> IProp (onLoad :: Event | r) i
onLoad = handler ET.load

onScroll :: forall r i. (Event -> Maybe i) -> IProp (onScroll :: Event | r) i
onScroll = handler (EventType "scroll")

onChange :: forall r i. (Event -> Maybe i) -> IProp (onChange :: Event | r) i
onChange = handler ET.change

onInput :: forall r i. (Event -> Maybe i) -> IProp (onInput :: Event | r) i
onInput = handler ET.input

onInvalid :: forall r i. (Event -> Maybe i) -> IProp (onInvalid :: Event | r) i
onInvalid = handler ET.invalid

onReset :: forall r i. (Event -> Maybe i) -> IProp (onReset :: Event | r) i
onReset = handler (EventType "reset")

onSelect :: forall r i. (Event -> Maybe i) -> IProp (onSelect :: Event | r) i
onSelect = handler ET.select

onSubmit :: forall r i. (Event -> Maybe i) -> IProp (onSubmit :: Event | r) i
onSubmit = handler (EventType "submit")

onTransitionEnd :: forall r i. (Event -> Maybe i) -> IProp (onTransitionEnd :: Event | r) i
onTransitionEnd = handler (EventType "transitionend")

onCopy :: forall r i. (ClipboardEvent -> Maybe i) -> IProp (onCopy :: ClipboardEvent | r) i
onCopy = handler CET.copy <<< clipboardHandler

onPaste :: forall r i. (ClipboardEvent -> Maybe i) -> IProp (onPaste :: ClipboardEvent | r) i
onPaste = handler CET.paste <<< clipboardHandler

onCut :: forall r i. (ClipboardEvent -> Maybe i) -> IProp (onCut :: ClipboardEvent | r) i
onCut = handler CET.cut <<< clipboardHandler

onClick :: forall r i. (MouseEvent -> Maybe i) -> IProp (onClick :: MouseEvent | r) i
onClick = handler MET.click <<< mouseHandler

-- onContextMenu :: forall r i. (MouseEvent -> Maybe i) -> IProp (onContextMenu :: MouseEvent | r) i
-- onContextMenu = handler ET.contextmenu <<< mouseHandler

onDoubleClick :: forall r i. (MouseEvent -> Maybe i) -> IProp (onDoubleClick :: MouseEvent | r) i
onDoubleClick = handler MET.dblclick <<< mouseHandler

onMouseDown :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseDown :: MouseEvent | r) i
onMouseDown = handler MET.mousedown <<< mouseHandler

onMouseEnter :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseEnter :: MouseEvent | r) i
onMouseEnter = handler MET.mouseenter <<< mouseHandler

onMouseLeave :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseLeave :: MouseEvent | r) i
onMouseLeave = handler MET.mouseleave <<< mouseHandler

onMouseMove :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseMove :: MouseEvent | r) i
onMouseMove = handler MET.mousemove <<< mouseHandler

onMouseOver :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseOver :: MouseEvent | r) i
onMouseOver = handler MET.mouseover <<< mouseHandler

onMouseOut :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseOut :: MouseEvent | r) i
onMouseOut = handler MET.mouseout <<< mouseHandler

onMouseUp :: forall r i. (MouseEvent -> Maybe i) -> IProp (onMouseUp :: MouseEvent | r) i
onMouseUp = handler MET.mouseup <<< mouseHandler

onWheel :: forall r i. (WheelEvent -> Maybe i) -> IProp (onWheel :: WheelEvent | r) i
onWheel = handler WET.wheel <<< wheelHandler

onKeyDown :: forall r i. (KeyboardEvent -> Maybe i) -> IProp (onKeyDown :: KeyboardEvent | r) i
onKeyDown = handler KET.keydown <<< keyHandler

-- onKeyPress :: forall r i. (KeyboardEvent -> Maybe i) -> IProp (onKeyPress :: KeyboardEvent | r) i
-- onKeyPress = handler KET.keypress <<< keyHandler

onKeyUp :: forall r i. (KeyboardEvent -> Maybe i) -> IProp (onKeyUp :: KeyboardEvent | r) i
onKeyUp = handler KET.keyup <<< keyHandler

onBlur :: forall r i. (FocusEvent -> Maybe i) -> IProp (onBlur :: FocusEvent | r) i
onBlur = handler ET.blur <<< focusHandler

onFocus :: forall r i. (FocusEvent -> Maybe i) -> IProp (onFocus :: FocusEvent | r) i
onFocus = handler FET.focus <<< focusHandler

onFocusIn :: forall r i. (FocusEvent -> Maybe i) -> IProp (onFocusIn :: FocusEvent | r) i
onFocusIn = handler FET.focusin <<< focusHandler

onFocusOut :: forall r i. (FocusEvent -> Maybe i) -> IProp (onFocusOut :: FocusEvent | r) i
onFocusOut = handler FET.focusout <<< focusHandler

onDrag :: forall r i. (DragEvent -> Maybe i) -> IProp (onDrag :: DragEvent | r) i
onDrag = handler DET.drag <<< dragHandler

onDragEnd :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragEnd :: DragEvent | r) i
onDragEnd = handler DET.dragend <<< dragHandler

onDragExit :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragExit :: DragEvent | r) i
onDragExit = handler DET.dragexit <<< dragHandler

onDragEnter :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragEnter :: DragEvent | r) i
onDragEnter = handler DET.dragenter <<< dragHandler

onDragLeave :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragLeave :: DragEvent | r) i
onDragLeave = handler DET.dragleave <<< dragHandler

onDragOver :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragOver :: DragEvent | r) i
onDragOver = handler DET.dragover <<< dragHandler

onDragStart :: forall r i. (DragEvent -> Maybe i) -> IProp (onDragStart :: DragEvent | r) i
onDragStart = handler DET.dragstart <<< dragHandler

onDrop :: forall r i. (DragEvent -> Maybe i) -> IProp (onDrop :: DragEvent | r) i
onDrop = handler DET.drop <<< dragHandler

onTouchCancel :: forall r i. (TouchEvent -> Maybe i) -> IProp (onTouchCancel :: TouchEvent | r) i
onTouchCancel = handler (EventType "touchcancel") <<< touchHandler

onTouchEnd :: forall r i. (TouchEvent -> Maybe i) -> IProp (onTouchEnd :: TouchEvent | r) i
onTouchEnd = handler (EventType "touchend") <<< touchHandler

onTouchEnter :: forall r i. (TouchEvent -> Maybe i) -> IProp (onTouchEnter :: TouchEvent | r) i
onTouchEnter = handler (EventType "touchenter") <<< touchHandler

onTouchLeave :: forall r i. (TouchEvent -> Maybe i) -> IProp (onTouchEnter :: TouchEvent | r) i
onTouchLeave = handler (EventType "touchleave") <<< touchHandler

onTouchMove :: forall r i. (TouchEvent -> Maybe i) -> IProp (onTouchMove :: TouchEvent | r) i
onTouchMove = handler (EventType "touchmove") <<< touchHandler

onTouchStart :: forall r i. (TouchEvent -> Maybe i) -> IProp (onTouchStart :: TouchEvent | r) i
onTouchStart = handler (EventType "touchstart") <<< touchHandler

onResize :: forall r i. (Event -> Maybe i) -> IProp (onResize :: Event | r) i
onResize = handler (EventType "resize")

keyHandler :: forall i. (KeyboardEvent -> Maybe i) -> Event -> Maybe i
keyHandler = unsafeCoerce

mouseHandler :: forall i. (MouseEvent -> Maybe i) -> Event -> Maybe i
mouseHandler = unsafeCoerce

wheelHandler :: forall i. (WheelEvent -> Maybe i) -> Event -> Maybe i
wheelHandler = unsafeCoerce

focusHandler :: forall i. (FocusEvent -> Maybe i) -> Event -> Maybe i
focusHandler = unsafeCoerce

dragHandler :: forall i. (DragEvent -> Maybe i) -> Event -> Maybe i
dragHandler = unsafeCoerce

clipboardHandler :: forall i. (ClipboardEvent -> Maybe i) -> Event -> Maybe i
clipboardHandler = unsafeCoerce

touchHandler :: forall i. (TouchEvent -> Maybe i) -> Event -> Maybe i
touchHandler = unsafeCoerce

-- | Attaches event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall r i value. EventType -> String -> (Foreign -> F value) -> (value -> Maybe i) -> IProp r i
addForeignPropHandler key prop reader f =
  handler key $ EE.currentTarget >=> \e -> either (const Nothing) f $ runExcept $ go e
  where
  go a = reader <=< readProp prop $ unsafeToForeign a

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall r i. (String -> Maybe i) -> IProp (value :: String, onChange :: Event | r) i
onValueChange = addForeignPropHandler ET.change "value" readString

-- | Attaches an event handler which will produce an input when the seleced index of a
-- | `select` element changes.
onSelectedIndexChange :: forall r i. (Int -> Maybe i) -> IProp (selectedIndex :: Int, onChange :: Event | r) i
onSelectedIndexChange = addForeignPropHandler ET.change "selectedIndex" readInt

-- | Attaches an event handler which will fire on input.
onValueInput :: forall r i. (String -> Maybe i) -> IProp (value :: String, onInput :: Event | r) i
onValueInput = addForeignPropHandler ET.input "value" readString

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall r i. (Boolean -> Maybe i) -> IProp (checked :: Boolean, onChange :: Event | r) i
onChecked = addForeignPropHandler ET.change "checked" readBoolean
