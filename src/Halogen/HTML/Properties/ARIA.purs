-- | This module provides `Prop` values for WAI-ARIA attributes.
module Halogen.HTML.Properties.ARIA
  ( activeDescendant
  , atomic
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

import Data.Maybe (Maybe(..))

import Halogen.HTML.Core (Prop(..), attrName)

activeDescendant :: forall i. String -> Prop i
activeDescendant = Attr Nothing (attrName "aria-activedescendant")

atomic :: forall i. String -> Prop i
atomic = Attr Nothing (attrName "aria-atomic")

autoComplete :: forall i. String -> Prop i
autoComplete = Attr Nothing (attrName "aria-autocomplete")

busy :: forall i. String -> Prop i
busy = Attr Nothing (attrName "aria-busy")

checked :: forall i. String -> Prop i
checked = Attr Nothing (attrName "aria-checked")

controls :: forall i. String -> Prop i
controls = Attr Nothing (attrName "aria-controls")

describedBy :: forall i. String -> Prop i
describedBy = Attr Nothing (attrName "aria-describedby")

disabled :: forall i. String -> Prop i
disabled = Attr Nothing (attrName "aria-disabled")

dropEffect :: forall i. String -> Prop i
dropEffect = Attr Nothing (attrName "aria-dropeffect")

expanded :: forall i. String -> Prop i
expanded = Attr Nothing (attrName "aria-expanded")

flowTo :: forall i. String -> Prop i
flowTo = Attr Nothing (attrName "aria-flowto")

grabbed :: forall i. String -> Prop i
grabbed = Attr Nothing (attrName "aria-grabbed")

hasPopup :: forall i. String -> Prop i
hasPopup = Attr Nothing (attrName "aria-haspopup")

hidden :: forall i. String -> Prop i
hidden = Attr Nothing (attrName "aria-hidden")

invalid :: forall i. String -> Prop i
invalid = Attr Nothing (attrName "aria-invalid")

label :: forall i. String -> Prop i
label = Attr Nothing (attrName "aria-label")

labelledBy :: forall i. String -> Prop i
labelledBy = Attr Nothing (attrName "aria-labelledby")

level :: forall i. String -> Prop i
level = Attr Nothing (attrName "aria-level")

live :: forall i. String -> Prop i
live = Attr Nothing (attrName "aria-live")

multiLine :: forall i. String -> Prop i
multiLine = Attr Nothing (attrName "aria-multiline")

multiSelectable :: forall i. String -> Prop i
multiSelectable = Attr Nothing (attrName "aria-multiselectable")

orientation :: forall i. String -> Prop i
orientation = Attr Nothing (attrName "aria-orientation")

owns :: forall i. String -> Prop i
owns = Attr Nothing (attrName "aria-owns")

posInSet :: forall i. String -> Prop i
posInSet = Attr Nothing (attrName "aria-posinset")

pressed :: forall i. String -> Prop i
pressed = Attr Nothing (attrName "aria-pressed")

readOnly :: forall i. String -> Prop i
readOnly = Attr Nothing (attrName "aria-readonly")

relevant :: forall i. String -> Prop i
relevant = Attr Nothing (attrName "aria-relevant")

required :: forall i. String -> Prop i
required = Attr Nothing (attrName "aria-required")

selected :: forall i. String -> Prop i
selected = Attr Nothing (attrName "aria-selected")

setSize :: forall i. String -> Prop i
setSize = Attr Nothing (attrName "aria-setsize")

sort :: forall i. String -> Prop i
sort = Attr Nothing (attrName "aria-sort")

valueMax :: forall i. String -> Prop i
valueMax = Attr Nothing (attrName "aria-valuemax")

valueMin :: forall i. String -> Prop i
valueMin = Attr Nothing (attrName "aria-valuemin")

valueNow :: forall i. String -> Prop i
valueNow = Attr Nothing (attrName "aria-valuenow")

valueText :: forall i. String -> Prop i
valueText = Attr Nothing (attrName "aria-valuetext")

role :: forall i. String -> Prop i
role = Attr Nothing (attrName "role")

