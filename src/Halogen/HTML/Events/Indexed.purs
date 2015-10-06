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

import Prelude
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events (input, input_) as ExportedEvents
import qualified Halogen.HTML.Events.Forms as E
import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Types (Event(), MouseEvent(), FocusEvent(), KeyboardEvent())
import Halogen.HTML.Core (Prop())
import Halogen.HTML.Properties.Indexed
import qualified Halogen.HTML.Properties.Indexed.Unsafe as Unsafe

type IEventProp r e i = (Event e -> EventHandler i) -> IProp r i

refine :: forall r i. Prop i -> IProp r i
refine = Unsafe.IProp

onAbort :: forall r i. IEventProp (onAbort :: I | r) () i
onAbort = refine <<< E.onAbort

onBeforeUnload :: forall r i. IEventProp (onBeforeUnload :: I | r) () i
onBeforeUnload = refine <<< E.onBeforeUnload

onError :: forall r i. IEventProp (onError :: I | r) () i
onError = refine <<< E.onError

onHashChange :: forall r i. IEventProp (onHashChange :: I | r) () i
onHashChange = refine <<< E.onHashChange

onLoad :: forall r i. IEventProp (onLoad :: I | r) () i
onLoad = refine <<< E.onLoad

onPageShow :: forall r i. IEventProp (onPageShow :: I | r) () i
onPageShow = refine <<< E.onPageShow

onPageHide :: forall r i. IEventProp (onPageHide :: I | r) () i
onPageHide = refine <<< E.onPageHide

onResize :: forall r i. IEventProp (onResize :: I | r) () i
onResize = refine <<< E.onResize

onScroll :: forall r i. IEventProp (onScroll :: I | r) () i
onScroll = refine <<< E.onScroll

onUnload :: forall r i. IEventProp (onUnload :: I | r) () i
onUnload = refine <<< E.onUnload

onChange :: forall r i. IEventProp (onChange :: I | r) () i
onChange = refine <<< E.onChange

onInput :: forall r i. IEventProp (onInput :: I | r) () i
onInput = refine <<< E.onInput

onInvalid :: forall r i. IEventProp (onInvalid :: I | r) () i
onInvalid = refine <<< E.onInvalid

onReset :: forall r i. IEventProp (onReset :: I | r) () i
onReset = refine <<< E.onReset

onSearch :: forall r i. IEventProp (onSearch :: I | r) () i
onSearch = refine <<< E.onSearch

onSelect :: forall r i. IEventProp (onSelect :: I | r) () i
onSelect = refine <<< E.onSelect

onSubmit :: forall r i. IEventProp (onSubmit :: I | r) () i
onSubmit = refine <<< E.onSubmit

onClick :: forall r i. IEventProp (onClick :: I | r) MouseEvent i
onClick = refine <<< E.onClick

onContextMenu :: forall r i. IEventProp (onContextMenu :: I | r) MouseEvent i
onContextMenu = refine <<< E.onContextMenu

onDoubleClick :: forall r i. IEventProp (onDoubleClick :: I | r) MouseEvent i
onDoubleClick = refine <<< E.onDoubleClick

onMouseDown :: forall r i. IEventProp (onMouseDown :: I | r) MouseEvent i
onMouseDown = refine <<< E.onMouseDown

onMouseLeave :: forall r i. IEventProp (onMouseLeave :: I | r) MouseEvent i
onMouseLeave = refine <<< E.onMouseLeave

onMouseMove :: forall r i. IEventProp (onMouseMove :: I | r) MouseEvent i
onMouseMove = refine <<< E.onMouseMove

onMouseOver :: forall r i. IEventProp (onMouseOver :: I | r) MouseEvent i
onMouseOver = refine <<< E.onMouseOver

onMouseOut :: forall r i. IEventProp (onMouseOut :: I | r) MouseEvent i
onMouseOut = refine <<< E.onMouseOut

onMouseUp :: forall r i. IEventProp (onMouseUp :: I | r) MouseEvent i
onMouseUp = refine <<< E.onMouseUp

onKeyDown :: forall r i. IEventProp (onKeyDown :: I | r) KeyboardEvent i
onKeyDown = refine <<< E.onKeyDown

onKeyPress :: forall r i. IEventProp (onKeyPress :: I | r) KeyboardEvent i
onKeyPress = refine <<< E.onKeyPress

onKeyUp :: forall r i. IEventProp (onKeyUp :: I | r) KeyboardEvent i
onKeyUp = refine <<< E.onKeyUp

onBlur :: forall r i. IEventProp (onBlur :: I | r) FocusEvent i
onBlur = refine <<< E.onBlur

onFocus :: forall r i. IEventProp (onFocus :: I | r) FocusEvent i
onFocus = refine <<< E.onFocus

onFocusIn :: forall r i. IEventProp (onFocusIn :: I | r) FocusEvent i
onFocusIn = refine <<< E.onFocusIn

onFocusOut :: forall r i. IEventProp (onFocusOut :: I | r) FocusEvent i
onFocusOut = refine <<< E.onFocusOut

onValueChange :: forall r f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onChange :: I | r) (f Unit)
onValueChange = refine <<< E.onValueChange

onValueInput :: forall r f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onInput :: I | r) (f Unit)
onValueInput = refine <<< E.onValueInput

onChecked :: forall r f. (Boolean -> EventHandler (f Unit)) -> IProp (checked :: I, onChange :: I | r) (f Unit)
onChecked = refine <<< E.onChecked
