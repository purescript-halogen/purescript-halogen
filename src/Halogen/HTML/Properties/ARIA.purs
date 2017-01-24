-- | This module provides properties for WAI-ARIA attributes.
module Halogen.HTML.Properties.ARIA where

import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Properties (IProp, attr)

activeDescendant :: forall r f p. String -> IProp r f p
activeDescendant = attr (AttrName "aria-activedescendant")

atomic :: forall r f p. String -> IProp r f p
atomic = attr (AttrName "aria-atomic")

autoComplete :: forall r f p. String -> IProp r f p
autoComplete = attr (AttrName "aria-autocomplete")

busy :: forall r f p. String -> IProp r f p
busy = attr (AttrName "aria-busy")

checked :: forall r f p. String -> IProp r f p
checked = attr (AttrName "aria-checked")

controls :: forall r f p. String -> IProp r f p
controls = attr (AttrName "aria-controls")

describedBy :: forall r f p. String -> IProp r f p
describedBy = attr (AttrName "aria-describedby")

disabled :: forall r f p. String -> IProp r f p
disabled = attr (AttrName "aria-disabled")

dropEffect :: forall r f p. String -> IProp r f p
dropEffect = attr (AttrName "aria-dropeffect")

expanded :: forall r f p. String -> IProp r f p
expanded = attr (AttrName "aria-expanded")

flowTo :: forall r f p. String -> IProp r f p
flowTo = attr (AttrName "aria-flowto")

grabbed :: forall r f p. String -> IProp r f p
grabbed = attr (AttrName "aria-grabbed")

hasPopup :: forall r f p. String -> IProp r f p
hasPopup = attr (AttrName "aria-haspopup")

hidden :: forall r f p. String -> IProp r f p
hidden = attr (AttrName "aria-hidden")

invalid :: forall r f p. String -> IProp r f p
invalid = attr (AttrName "aria-invalid")

label :: forall r f p. String -> IProp r f p
label = attr (AttrName "aria-label")

labelledBy :: forall r f p. String -> IProp r f p
labelledBy = attr (AttrName "aria-labelledby")

level :: forall r f p. String -> IProp r f p
level = attr (AttrName "aria-level")

live :: forall r f p. String -> IProp r f p
live = attr (AttrName "aria-live")

multiLine :: forall r f p. String -> IProp r f p
multiLine = attr (AttrName "aria-multiline")

multiSelectable :: forall r f p. String -> IProp r f p
multiSelectable = attr (AttrName "aria-multiselectable")

orientation :: forall r f p. String -> IProp r f p
orientation = attr (AttrName "aria-orientation")

owns :: forall r f p. String -> IProp r f p
owns = attr (AttrName "aria-owns")

posInSet :: forall r f p. String -> IProp r f p
posInSet = attr (AttrName "aria-posinset")

pressed :: forall r f p. String -> IProp r f p
pressed = attr (AttrName "aria-pressed")

readOnly :: forall r f p. String -> IProp r f p
readOnly = attr (AttrName "aria-readonly")

relevant :: forall r f p. String -> IProp r f p
relevant = attr (AttrName "aria-relevant")

required :: forall r f p. String -> IProp r f p
required = attr (AttrName "aria-required")

selected :: forall r f p. String -> IProp r f p
selected = attr (AttrName "aria-selected")

setSize :: forall r f p. String -> IProp r f p
setSize = attr (AttrName "aria-setsize")

sort :: forall r f p. String -> IProp r f p
sort = attr (AttrName "aria-sort")

valueMax :: forall r f p. String -> IProp r f p
valueMax = attr (AttrName "aria-valuemax")

valueMin :: forall r f p. String -> IProp r f p
valueMin = attr (AttrName "aria-valuemin")

valueNow :: forall r f p. String -> IProp r f p
valueNow = attr (AttrName "aria-valuenow")

valueText :: forall r f p. String -> IProp r f p
valueText = attr (AttrName "aria-valuetext")

role :: forall r f p. String -> IProp r f p
role = attr (AttrName "role")
