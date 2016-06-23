module Halogen.HTML.Properties.Indexed.ARIA
  ( atomic
  , autoComplete
  , busy
  , checked
  , controls
  , describedBy
  , disabled
  , dropEffect
  , expanded
  , flowTo
  , grabbed
  , hasPopup
  , hidden
  , invalid
  , label
  , labelledBy
  , level
  , live
  , multiLine
  , multiSelectable
  , orientation
  , owns
  , posInSet
  , pressed
  , readOnly
  , relevant
  , required
  , selected
  , setSize
  , sort
  , valueMax
  , valueMin
  , valueNow
  , valueText
  , role
  ) where

import Unsafe.Coerce (unsafeCoerce)

import Halogen.HTML.Core (Prop())
import Halogen.HTML.Properties.Indexed (IProp())
import Halogen.HTML.Properties.ARIA as P

refine :: forall a r i. (a -> Prop i) -> a -> IProp r i
refine = unsafeCoerce

activeDescendant :: forall r i. String -> IProp r i
activeDescendant = refine P.activeDescendant

atomic :: forall r i. String -> IProp r i
atomic = refine P.atomic

autoComplete :: forall r i. String -> IProp r i
autoComplete = refine P.autoComplete

busy :: forall r i. String -> IProp r i
busy = refine P.busy

checked :: forall r i. String -> IProp r i
checked = refine P.checked

controls :: forall r i. String -> IProp r i
controls = refine P.controls

describedBy :: forall r i. String -> IProp r i
describedBy = refine P.describedBy

disabled :: forall r i. String -> IProp r i
disabled = refine P.disabled

dropEffect :: forall r i. String -> IProp r i
dropEffect = refine P.dropEffect

expanded :: forall r i. String -> IProp r i
expanded = refine P.expanded

flowTo :: forall r i. String -> IProp r i
flowTo = refine P.flowTo

grabbed :: forall r i. String -> IProp r i
grabbed = refine P.grabbed

hasPopup :: forall r i. String -> IProp r i
hasPopup = refine P.hasPopup

hidden :: forall r i. String -> IProp r i
hidden = refine P.hidden

invalid :: forall r i. String -> IProp r i
invalid = refine P.invalid

label :: forall r i. String -> IProp r i
label = refine P.label

labelledBy :: forall r i. String -> IProp r i
labelledBy = refine P.labelledBy

level :: forall r i. String -> IProp r i
level = refine P.level

live :: forall r i. String -> IProp r i
live = refine P.live

multiLine :: forall r i. String -> IProp r i
multiLine = refine P.multiLine

multiSelectable :: forall r i. String -> IProp r i
multiSelectable = refine P.multiSelectable

orientation :: forall r i. String -> IProp r i
orientation = refine P.orientation

owns :: forall r i. String -> IProp r i
owns = refine P.owns

posInSet :: forall r i. String -> IProp r i
posInSet = refine P.posInSet

pressed :: forall r i. String -> IProp r i
pressed = refine P.pressed

readOnly :: forall r i. String -> IProp r i
readOnly = refine P.readOnly

relevant :: forall r i. String -> IProp r i
relevant = refine P.relevant

required :: forall r i. String -> IProp r i
required = refine P.required

selected :: forall r i. String -> IProp r i
selected = refine P.selected

setSize :: forall r i. String -> IProp r i
setSize = refine P.setSize

sort :: forall r i. String -> IProp r i
sort = refine P.sort

valueMax :: forall r i. String -> IProp r i
valueMax = refine P.valueMax

valueMin :: forall r i. String -> IProp r i
valueMin = refine P.valueMin

valueNow :: forall r i. String -> IProp r i
valueNow = refine P.valueNow

valueText :: forall r i. String -> IProp r i
valueText = refine P.valueText

role :: forall r i. String -> IProp r i
role = refine P.role

