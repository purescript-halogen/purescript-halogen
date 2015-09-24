-- | This module provides `Prop` values for some common HTML attributes.
module Halogen.HTML.Properties
  ( key
  , alt
  , charset
  , class_
  , classes
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
  , LengthLiteral(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

import Halogen.HTML.Core (Prop(..), ClassName(), IsProp, prop, propName, attrName, runClassName)
import Halogen.HTML.Events.Types (Event())
import Halogen.HTML.Events.Handler (EventHandler())

data LengthLiteral
  = Pixels Number
  | Percent Number

instance lengthLiteralIsProp :: IsProp LengthLiteral where
  toPropString _ _ lit =
    case lit of
      Pixels n -> show n
      Percent n -> show n ++ "%"

-- | The `key` property associates a unique key with a node, which can be used
-- | to implement a more efficient diff/patch.
key :: forall i. String -> Prop i
key = Key

alt :: forall i. String -> Prop i
alt = prop (propName "alt") (Just $ attrName "alt")

charset :: forall i. String -> Prop i
charset = prop (propName "charset") (Just $ attrName "charset")

class_ :: forall i. ClassName -> Prop i
class_ = prop (propName "className") (Just $ attrName "class") <<< runClassName

classes :: forall i. Array ClassName -> Prop i
classes = prop (propName "className") (Just $ attrName "class") <<< joinWith " " <<< map runClassName

colSpan :: forall i. Int -> Prop i
colSpan = prop (propName "colSpan") (Just $ attrName "colspan")

rowSpan :: forall i. Int -> Prop i
rowSpan = prop (propName "rowSpan") (Just $ attrName "rowspan")

for :: forall i. String -> Prop i
for = prop (propName "htmlFor") (Just $ attrName "for")

height :: forall i. LengthLiteral -> Prop i
height = prop (propName "height") (Just $ attrName "height")

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
width = prop (propName "width") (Just $ attrName "width")

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
