module Halogen.HTML.Events.Indexed
  ( IEventProp()
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
  , onValueChange
  , onValueInput
  , onChecked
  , module ExportedEvents
  ) where

import Prelude (..)

import Unsafe.Coerce (unsafeCoerce)

import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Types (Event(), MouseEvent(), FocusEvent(), KeyboardEvent())
import Halogen.HTML.Properties.Indexed (IProp(), I())
import Halogen.HTML.Events (input, input_) as ExportedEvents
import Halogen.HTML.Events (..) as E
import Halogen.HTML.Events.Forms (..) as E

type IEventProp r e i = (Event e -> EventHandler i) -> IProp r i

onAbort :: forall r i. IEventProp (onAbort :: I | r) () i
onAbort = unsafeCoerce E.onAbort

onBeforeUnload :: forall r i. IEventProp (onBeforeUnload :: I | r) () i
onBeforeUnload = unsafeCoerce E.onBeforeUnload

onError :: forall r i. IEventProp (onError :: I | r) () i
onError = unsafeCoerce E.onError

onHashChange :: forall r i. IEventProp (onHashChange :: I | r) () i
onHashChange = unsafeCoerce E.onHashChange

onLoad :: forall r i. IEventProp (onLoad :: I | r) () i
onLoad = unsafeCoerce E.onLoad

onPageShow :: forall r i. IEventProp (onPageShow :: I | r) () i
onPageShow = unsafeCoerce E.onPageShow

onPageHide :: forall r i. IEventProp (onPageHide :: I | r) () i
onPageHide = unsafeCoerce E.onPageHide

onResize :: forall r i. IEventProp (onResize :: I | r) () i
onResize = unsafeCoerce E.onResize

onScroll :: forall r i. IEventProp (onScroll :: I | r) () i
onScroll = unsafeCoerce E.onScroll

onUnload :: forall r i. IEventProp (onUnload :: I | r) () i
onUnload = unsafeCoerce E.onUnload

onChange :: forall r i. IEventProp (onChange :: I | r) () i
onChange = unsafeCoerce E.onChange

onInput :: forall r i. IEventProp (onInput :: I | r) () i
onInput = unsafeCoerce E.onInput

onInvalid :: forall r i. IEventProp (onInvalid :: I | r) () i
onInvalid = unsafeCoerce E.onInvalid

onReset :: forall r i. IEventProp (onReset :: I | r) () i
onReset = unsafeCoerce E.onReset

onSearch :: forall r i. IEventProp (onSearch :: I | r) () i
onSearch = unsafeCoerce E.onSearch

onSelect :: forall r i. IEventProp (onSelect :: I | r) () i
onSelect = unsafeCoerce E.onSelect

onSubmit :: forall r i. IEventProp (onSubmit :: I | r) () i
onSubmit = unsafeCoerce E.onSubmit

onClick :: forall r i. IEventProp (onClick :: I | r) MouseEvent i
onClick = unsafeCoerce E.onClick

onContextMenu :: forall r i. IEventProp (onContextMenu :: I | r) MouseEvent i
onContextMenu = unsafeCoerce E.onContextMenu

onDoubleClick :: forall r i. IEventProp (onDoubleClick :: I | r) MouseEvent i
onDoubleClick = unsafeCoerce E.onDoubleClick

onMouseDown :: forall r i. IEventProp (onMouseDown :: I | r) MouseEvent i
onMouseDown = unsafeCoerce E.onMouseDown

onMouseEnter :: forall r i. IEventProp (onMouseEnter :: I | r) MouseEvent i
onMouseEnter = unsafeCoerce E.onMouseEnter

onMouseLeave :: forall r i. IEventProp (onMouseLeave :: I | r) MouseEvent i
onMouseLeave = unsafeCoerce E.onMouseLeave

onMouseMove :: forall r i. IEventProp (onMouseMove :: I | r) MouseEvent i
onMouseMove = unsafeCoerce E.onMouseMove

onMouseOver :: forall r i. IEventProp (onMouseOver :: I | r) MouseEvent i
onMouseOver = unsafeCoerce E.onMouseOver

onMouseOut :: forall r i. IEventProp (onMouseOut :: I | r) MouseEvent i
onMouseOut = unsafeCoerce E.onMouseOut

onMouseUp :: forall r i. IEventProp (onMouseUp :: I | r) MouseEvent i
onMouseUp = unsafeCoerce E.onMouseUp

onKeyDown :: forall r i. IEventProp (onKeyDown :: I | r) KeyboardEvent i
onKeyDown = unsafeCoerce E.onKeyDown

onKeyPress :: forall r i. IEventProp (onKeyPress :: I | r) KeyboardEvent i
onKeyPress = unsafeCoerce E.onKeyPress

onKeyUp :: forall r i. IEventProp (onKeyUp :: I | r) KeyboardEvent i
onKeyUp = unsafeCoerce E.onKeyUp

onBlur :: forall r i. IEventProp (onBlur :: I | r) FocusEvent i
onBlur = unsafeCoerce E.onBlur

onFocus :: forall r i. IEventProp (onFocus :: I | r) FocusEvent i
onFocus = unsafeCoerce E.onFocus

onFocusIn :: forall r i. IEventProp (onFocusIn :: I | r) FocusEvent i
onFocusIn = unsafeCoerce E.onFocusIn

onFocusOut :: forall r i. IEventProp (onFocusOut :: I | r) FocusEvent i
onFocusOut = unsafeCoerce E.onFocusOut

onValueChange :: forall r f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onChange :: I | r) (f Unit)
onValueChange = unsafeCoerce E.onValueChange

onValueInput :: forall r f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onInput :: I | r) (f Unit)
onValueInput = unsafeCoerce E.onValueInput

onChecked :: forall r f. (Boolean -> EventHandler (f Unit)) -> IProp (checked :: I, onChange :: I | r) (f Unit)
onChecked = unsafeCoerce E.onChecked
