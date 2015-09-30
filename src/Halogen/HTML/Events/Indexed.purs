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

type IEventProp ρ e i = (Event e -> EventHandler i) -> IProp ρ i

refine :: forall ρ i. Prop i -> IProp ρ i
refine = Unsafe.IProp

onAbort :: forall ρ i. IEventProp (onAbort :: I | ρ) () i
onAbort = refine <<< E.onAbort

onBeforeUnload :: forall ρ i. IEventProp (onBeforeUnload :: I | ρ) () i
onBeforeUnload = refine <<< E.onBeforeUnload

onError :: forall ρ i. IEventProp (onError :: I | ρ) () i
onError = refine <<< E.onError

onHashChange :: forall ρ i. IEventProp (onHashChange :: I | ρ) () i
onHashChange = refine <<< E.onHashChange

onLoad :: forall ρ i. IEventProp (onLoad :: I | ρ) () i
onLoad = refine <<< E.onLoad

onPageShow :: forall ρ i. IEventProp (onPageShow :: I | ρ) () i
onPageShow = refine <<< E.onPageShow

onPageHide :: forall ρ i. IEventProp (onPageHide :: I | ρ) () i
onPageHide = refine <<< E.onPageHide

onResize :: forall ρ i. IEventProp (onResize :: I | ρ) () i
onResize = refine <<< E.onResize

onScroll :: forall ρ i. IEventProp (onScroll :: I | ρ) () i
onScroll = refine <<< E.onScroll

onUnload :: forall ρ i. IEventProp (onUnload :: I | ρ) () i
onUnload = refine <<< E.onUnload

onChange :: forall ρ i. IEventProp (onChange :: I | ρ) () i
onChange = refine <<< E.onChange

onInput :: forall ρ i. IEventProp (onInput :: I | ρ) () i
onInput = refine <<< E.onInput

onInvalid :: forall ρ i. IEventProp (onInvalid :: I | ρ) () i
onInvalid = refine <<< E.onInvalid

onReset :: forall ρ i. IEventProp (onReset :: I | ρ) () i
onReset = refine <<< E.onReset

onSearch :: forall ρ i. IEventProp (onSearch :: I | ρ) () i
onSearch = refine <<< E.onSearch

onSelect :: forall ρ i. IEventProp (onSelect :: I | ρ) () i
onSelect = refine <<< E.onSelect

onSubmit :: forall ρ i. IEventProp (onSubmit :: I | ρ) () i
onSubmit = refine <<< E.onSubmit

onClick :: forall ρ i. IEventProp (onClick :: I | ρ) MouseEvent i
onClick = refine <<< E.onClick

onContextMenu :: forall ρ i. IEventProp (onContextMenu :: I | ρ) MouseEvent i
onContextMenu = refine <<< E.onContextMenu

onDoubleClick :: forall ρ i. IEventProp (onDoubleClick :: I | ρ) MouseEvent i
onDoubleClick = refine <<< E.onDoubleClick

onMouseDown :: forall ρ i. IEventProp (onMouseDown :: I | ρ) MouseEvent i
onMouseDown = refine <<< E.onMouseDown

onMouseLeave :: forall ρ i. IEventProp (onMouseLeave :: I | ρ) MouseEvent i
onMouseLeave = refine <<< E.onMouseLeave

onMouseMove :: forall ρ i. IEventProp (onMouseMove :: I | ρ) MouseEvent i
onMouseMove = refine <<< E.onMouseMove

onMouseOver :: forall ρ i. IEventProp (onMouseOver :: I | ρ) MouseEvent i
onMouseOver = refine <<< E.onMouseOver

onMouseOut :: forall ρ i. IEventProp (onMouseOut :: I | ρ) MouseEvent i
onMouseOut = refine <<< E.onMouseOut

onMouseUp :: forall ρ i. IEventProp (onMouseUp :: I | ρ) MouseEvent i
onMouseUp = refine <<< E.onMouseUp

onKeyDown :: forall ρ i. IEventProp (onKeyDown :: I | ρ) KeyboardEvent i
onKeyDown = refine <<< E.onKeyDown

onKeyPress :: forall ρ i. IEventProp (onKeyPress :: I | ρ) KeyboardEvent i
onKeyPress = refine <<< E.onKeyPress

onKeyUp :: forall ρ i. IEventProp (onKeyUp :: I | ρ) KeyboardEvent i
onKeyUp = refine <<< E.onKeyUp

onBlur :: forall ρ i. IEventProp (onBlur :: I | ρ) FocusEvent i
onBlur = refine <<< E.onBlur

onFocus :: forall ρ i. IEventProp (onFocus :: I | ρ) FocusEvent i
onFocus = refine <<< E.onFocus

onFocusIn :: forall ρ i. IEventProp (onFocusIn :: I | ρ) FocusEvent i
onFocusIn = refine <<< E.onFocusIn

onFocusOut :: forall ρ i. IEventProp (onFocusOut :: I | ρ) FocusEvent i
onFocusOut = refine <<< E.onFocusOut

onValueChange :: forall ρ f. (String -> EventHandler (f Unit)) -> IProp (value :: I | ρ) (f Unit)
onValueChange = refine <<< E.onValueChange

onValueInput :: forall ρ f. (String -> EventHandler (f Unit)) -> IProp (value :: I | ρ) (f Unit)
onValueInput = refine <<< E.onValueInput

onChecked :: forall ρ f. (Boolean -> EventHandler (f Unit)) -> IProp (checked :: I | ρ) (f Unit)
onChecked = refine <<< E.onChecked
