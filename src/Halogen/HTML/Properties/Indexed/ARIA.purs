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

import Prelude

import Unsafe.Coerce (unsafeCoerce)

import Halogen.HTML.Properties.Indexed (IProp())
import qualified Halogen.HTML.Properties.ARIA as P

activeDescendant :: forall r i. String -> IProp r i
activeDescendant = unsafeCoerce P.activeDescendant

atomic :: forall r i. String -> IProp r i
atomic = unsafeCoerce P.atomic

autoComplete :: forall r i. String -> IProp r i
autoComplete = unsafeCoerce P.autoComplete

busy :: forall r i. String -> IProp r i
busy = unsafeCoerce P.busy

checked :: forall r i. String -> IProp r i
checked = unsafeCoerce P.checked

controls :: forall r i. String -> IProp r i
controls = unsafeCoerce P.controls

describedBy :: forall r i. String -> IProp r i
describedBy = unsafeCoerce P.describedBy

disabled :: forall r i. String -> IProp r i
disabled = unsafeCoerce P.disabled

dropEffect :: forall r i. String -> IProp r i
dropEffect = unsafeCoerce P.dropEffect

expanded :: forall r i. String -> IProp r i
expanded = unsafeCoerce P.expanded

flowTo :: forall r i. String -> IProp r i
flowTo = unsafeCoerce P.flowTo

grabbed :: forall r i. String -> IProp r i
grabbed = unsafeCoerce P.grabbed

hasPopup :: forall r i. String -> IProp r i
hasPopup = unsafeCoerce P.hasPopup

hidden :: forall r i. String -> IProp r i
hidden = unsafeCoerce P.hidden

invalid :: forall r i. String -> IProp r i
invalid = unsafeCoerce P.invalid

label :: forall r i. String -> IProp r i
label = unsafeCoerce P.label

labelledBy :: forall r i. String -> IProp r i
labelledBy = unsafeCoerce P.labelledBy

level :: forall r i. String -> IProp r i
level = unsafeCoerce P.level

live :: forall r i. String -> IProp r i
live = unsafeCoerce P.live

multiLine :: forall r i. String -> IProp r i
multiLine = unsafeCoerce P.multiLine

multiSelectable :: forall r i. String -> IProp r i
multiSelectable = unsafeCoerce P.multiSelectable

orientation :: forall r i. String -> IProp r i
orientation = unsafeCoerce P.orientation

owns :: forall r i. String -> IProp r i
owns = unsafeCoerce P.owns

posInSet :: forall r i. String -> IProp r i
posInSet = unsafeCoerce P.posInSet

pressed :: forall r i. String -> IProp r i
pressed = unsafeCoerce P.pressed

readOnly :: forall r i. String -> IProp r i
readOnly = unsafeCoerce P.readOnly

relevant :: forall r i. String -> IProp r i
relevant = unsafeCoerce P.relevant

required :: forall r i. String -> IProp r i
required = unsafeCoerce P.required

selected :: forall r i. String -> IProp r i
selected = unsafeCoerce P.selected

setSize :: forall r i. String -> IProp r i
setSize = unsafeCoerce P.setSize

sort :: forall r i. String -> IProp r i
sort = unsafeCoerce P.sort

valueMax :: forall r i. String -> IProp r i
valueMax = unsafeCoerce P.valueMax

valueMin :: forall r i. String -> IProp r i
valueMin = unsafeCoerce P.valueMin

valueNow :: forall r i. String -> IProp r i
valueNow = unsafeCoerce P.valueNow

valueText :: forall r i. String -> IProp r i
valueText = unsafeCoerce P.valueText

role :: forall r i. String -> IProp r i
role = unsafeCoerce P.role

