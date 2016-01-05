-- | This module provides `Prop` values for some common HTML attributes.
module Halogen.HTML.Properties
  ( key
  , alt
  , ariaActivedescendant
  , ariaAtomic
  , ariaAutocomplete
  , ariaBusy
  , ariaChecked
  , ariaControls
  , ariaDescribedby
  , ariaDisabled
  , ariaDropeffect
  , ariaExpanded
  , ariaFlowto
  , ariaGrabbed
  , ariaHaspopup
  , ariaHidden
  , ariaInvalid
  , ariaLabel
  , ariaLabelledby
  , ariaLevel
  , ariaLive
  , ariaMultiline
  , ariaMultiselectable
  , ariaOrientation
  , ariaOwns
  , ariaPosinset
  , ariaPressed
  , ariaReadonly
  , ariaRelevant
  , ariaRequired
  , ariaSelected
  , ariaSetsize
  , ariaSort
  , ariaValuemax
  , ariaValuemin
  , ariaValuenow
  , ariaValuetext
  , charset
  , class_
  , classes
  , cols
  , rows
  , colSpan
  , rowSpan
  , for
  , height
  , href
  , id_
  , name
  , rel
  , src
  , target
  , title
  , type_
  , value
  , width
  , disabled
  , required
  , readonly
  , spellcheck
  , enabled
  , checked
  , selected
  , placeholder
  , autocomplete
  , autofocus
  , initializer
  , finalizer
  , LengthLiteral(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

import DOM.HTML.Types (HTMLElement())

import Halogen.HTML.Core (Prop(..), ClassName(), IsProp, prop, propName, attrName, runClassName)

data LengthLiteral
  = Pixels Int
  | Percent Number

printLengthLiteral :: LengthLiteral -> String
printLengthLiteral (Pixels n) = show n
printLengthLiteral (Percent n) = show n ++ "%"

-- | The `key` property associates a unique key with a node, which can be used
-- | to implement a more efficient diff/patch.
key :: forall i. String -> Prop i
key = Key

ariaActivedescendant :: forall i. String -> Prop i
ariaActivedescendant = prop (propName "aria-activedescendant") (Just $ attrName "aria-activedescendant")

ariaAtomic :: forall i. String -> Prop i
ariaAtomic = prop (propName "aria-atomic") (Just $ attrName "aria-atomic")

ariaAutocomplete :: forall i. String -> Prop i
ariaAutocomplete = prop (propName "aria-autocomplete") (Just $ attrName "aria-autocomplete")

ariaBusy :: forall i. String -> Prop i
ariaBusy = prop (propName "aria-busy") (Just $ attrName "aria-busy")

ariaChecked :: forall i. String -> Prop i
ariaChecked = prop (propName "aria-checked") (Just $ attrName "aria-checked")

ariaControls :: forall i. String -> Prop i
ariaControls = prop (propName "aria-controls") (Just $ attrName "aria-controls")

ariaDescribedby :: forall i. String -> Prop i
ariaDescribedby = prop (propName "aria-describedby") (Just $ attrName "aria-describedby")

ariaDisabled :: forall i. String -> Prop i
ariaDisabled = prop (propName "aria-disabled") (Just $ attrName "aria-disabled")

ariaDropeffect :: forall i. String -> Prop i
ariaDropeffect = prop (propName "aria-dropeffect") (Just $ attrName "aria-dropeffect")

ariaExpanded :: forall i. String -> Prop i
ariaExpanded = prop (propName "aria-expanded") (Just $ attrName "aria-expanded")

ariaFlowto :: forall i. String -> Prop i
ariaFlowto = prop (propName "aria-flowto") (Just $ attrName "aria-flowto")

ariaGrabbed :: forall i. String -> Prop i
ariaGrabbed = prop (propName "aria-grabbed") (Just $ attrName "aria-grabbed")

ariaHaspopup :: forall i. String -> Prop i
ariaHaspopup = prop (propName "aria-haspopup") (Just $ attrName "aria-haspopup")

ariaHidden :: forall i. String -> Prop i
ariaHidden = prop (propName "aria-hidden") (Just $ attrName "aria-hidden")

ariaInvalid :: forall i. String -> Prop i
ariaInvalid = prop (propName "aria-invalid") (Just $ attrName "aria-invalid")

ariaLabel :: forall i. String -> Prop i
ariaLabel = prop (propName "aria-label") (Just $ attrName "aria-label")

ariaLabelledby :: forall i. String -> Prop i
ariaLabelledby = prop (propName "aria-labelledby") (Just $ attrName "aria-labelledby")

ariaLevel :: forall i. String -> Prop i
ariaLevel = prop (propName "aria-level") (Just $ attrName "aria-level")

ariaLive :: forall i. String -> Prop i
ariaLive = prop (propName "aria-live") (Just $ attrName "aria-live")

ariaMultiline :: forall i. String -> Prop i
ariaMultiline = prop (propName "aria-multiline") (Just $ attrName "aria-multiline")

ariaMultiselectable :: forall i. String -> Prop i
ariaMultiselectable = prop (propName "aria-multiselectable") (Just $ attrName "aria-multiselectable")

ariaOrientation :: forall i. String -> Prop i
ariaOrientation = prop (propName "aria-orientation") (Just $ attrName "aria-orientation")

ariaOwns :: forall i. String -> Prop i
ariaOwns = prop (propName "aria-owns") (Just $ attrName "aria-owns")

ariaPosinset :: forall i. String -> Prop i
ariaPosinset = prop (propName "aria-posinset") (Just $ attrName "aria-posinset")

ariaPressed :: forall i. String -> Prop i
ariaPressed = prop (propName "aria-pressed") (Just $ attrName "aria-pressed")

ariaReadonly :: forall i. String -> Prop i
ariaReadonly = prop (propName "aria-readonly") (Just $ attrName "aria-readonly")

ariaRelevant :: forall i. String -> Prop i
ariaRelevant = prop (propName "aria-relevant") (Just $ attrName "aria-relevant")

ariaRequired :: forall i. String -> Prop i
ariaRequired = prop (propName "aria-required") (Just $ attrName "aria-required")

ariaSelected :: forall i. String -> Prop i
ariaSelected = prop (propName "aria-selected") (Just $ attrName "aria-selected")

ariaSetsize :: forall i. String -> Prop i
ariaSetsize = prop (propName "aria-setsize") (Just $ attrName "aria-setsize")

ariaSort :: forall i. String -> Prop i
ariaSort = prop (propName "aria-sort") (Just $ attrName "aria-sort")

ariaValuemax :: forall i. String -> Prop i
ariaValuemax = prop (propName "aria-valuemax") (Just $ attrName "aria-valuemax")

ariaValuemin :: forall i. String -> Prop i
ariaValuemin = prop (propName "aria-valuemin") (Just $ attrName "aria-valuemin")

ariaValuenow :: forall i. String -> Prop i
ariaValuenow = prop (propName "aria-valuenow") (Just $ attrName "aria-valuenow")

ariaValuetext :: forall i. String -> Prop i
ariaValuetext = prop (propName "aria-valuetext") (Just $ attrName "aria-valuetext")

alt :: forall i. String -> Prop i
alt = prop (propName "alt") (Just $ attrName "alt")

charset :: forall i. String -> Prop i
charset = prop (propName "charset") (Just $ attrName "charset")

class_ :: forall i. ClassName -> Prop i
class_ = prop (propName "className") (Just $ attrName "class") <<< runClassName

classes :: forall i. Array ClassName -> Prop i
classes = prop (propName "className") (Just $ attrName "class") <<< joinWith " " <<< map runClassName

cols :: forall i. Int -> Prop i
cols = prop (propName "cols") (Just $ attrName "cols")

rows :: forall i. Int -> Prop i
rows = prop (propName "rows") (Just $ attrName "rows")

colSpan :: forall i. Int -> Prop i
colSpan = prop (propName "colSpan") (Just $ attrName "colspan")

rowSpan :: forall i. Int -> Prop i
rowSpan = prop (propName "rowSpan") (Just $ attrName "rowspan")

for :: forall i. String -> Prop i
for = prop (propName "htmlFor") (Just $ attrName "for")

height :: forall i. LengthLiteral -> Prop i
height = prop (propName "height") (Just $ attrName "height") <<< printLengthLiteral

href :: forall i. String -> Prop i
href = prop (propName "href") (Just $ attrName "href")

id_ :: forall i. String -> Prop i
id_ = prop (propName "id") (Just $ attrName "id")

name :: forall i. String -> Prop i
name = prop (propName "name") (Just $ attrName "name")

rel :: forall i. String -> Prop i
rel = prop (propName "rel") (Just $ attrName "rel")

src :: forall i. String -> Prop i
src = prop (propName "src") (Just $ attrName "src")

target :: forall i. String -> Prop i
target = prop (propName "target") (Just $ attrName "target")

title :: forall i. String -> Prop i
title = prop (propName "title") (Just $ attrName "title")

type_ :: forall i. String -> Prop i
type_ = prop (propName "type") (Just $ attrName "type")

-- TODO: string rendering of `value` will need custom handling depending on element type
value :: forall i. String -> Prop i
value = prop (propName "value") (Just $ attrName "value")

width :: forall i. LengthLiteral -> Prop i
width = prop (propName "width") (Just $ attrName "width") <<< printLengthLiteral

disabled :: forall i. Boolean -> Prop i
disabled = prop (propName "disabled") (Just $ attrName "disabled")

required :: forall i. Boolean -> Prop i
required = prop (propName "required") (Just $ attrName "required")

readonly :: forall i. Boolean -> Prop i
readonly = prop (propName "readonly") (Just $ attrName "readonly")

spellcheck :: forall i. Boolean -> Prop i
spellcheck = prop (propName "spellcheck") (Just $ attrName "spellcheck")

enabled :: forall i. Boolean -> Prop i
enabled = disabled <<< not

checked :: forall i. Boolean -> Prop i
checked = prop (propName "checked") (Just $ attrName "checked")

selected :: forall i. Boolean -> Prop i
selected = prop (propName "selected") (Just $ attrName "selected")

placeholder :: forall i. String -> Prop i
placeholder = prop (propName "placeholder") (Just $ attrName "placeholder")

autocomplete :: forall i. Boolean -> Prop i
autocomplete = prop (propName "autocomplete") (Just $ attrName "autocomplete") <<< (\b -> if b then "on" else "off")

autofocus :: forall i. Boolean -> Prop i
autofocus = prop (propName "autofocus") (Just $ attrName "autofocus")

initializer :: forall i. (HTMLElement -> i) -> Prop i
initializer = Initializer

finalizer :: forall i. (HTMLElement -> i) -> Prop i
finalizer = Finalizer
