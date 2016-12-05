-- | A closed signature of type-indexed (refined) HTML properties; these can be
-- | used to ensure correctness by construction, and then erased into the
-- | standard unrefined versions.
module Halogen.HTML.Properties
  ( IProp(..)
  , I
  , prop
  , attr

  , ButtonType(..)
  , InputType(..)
  , MenuType(..)
  , MenuitemType(..)
  , OrderedListType(..)
  , NumeralType(..)
  , CaseType(..)

  , alt
  , charset
  , class_, classes
  , cols
  , rows
  , colSpan
  , rowSpan
  , for
  , height
  , width
  , href
  , id_
  , name
  , rel
  , src
  , target
  , title

  , FormMethod(..)
  , method
  , action
  , enctype
  , novalidate

  , buttonType
  , inputType
  , mediaType
  , menuType
  , menuitemType
  , olType

  , value
  , disabled
  , enabled
  , required
  , readonly
  , spellcheck
  , checked
  , selected
  , placeholder
  , autocomplete
  , autofocus
  , multiple

  , draggable
  , tabIndex

  , ref

  , LengthLiteral(..)
  , printLengthLiteral

  , GlobalAttributes
  , GlobalEvents
  , MouseEvents
  , DragEvents
  , TouchEvents
  , PointerEvents
  , KeyEvents
  , FocusEvents
  , TransitionEvents
  , InteractiveEvents
  , GlobalProperties
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (lmap)
import Data.String (joinWith)

import DOM.HTML.Types (HTMLElement)

import Halogen.HTML.Core (class IsProp, ClassName, AttrName(..), PropName(..), Prop(..), ElemRef(..))
import Halogen.HTML.Core as Core

import Unsafe.Coerce (unsafeCoerce)

-- | The phantom row `r` can be thought of as a context which is synthesized in
-- | the course of constructing a refined HTML expression.
newtype IProp (r :: # *) i = IProp (Prop i)

derive instance newtypeIProp :: Newtype (IProp r i) _

-- | A dummy type to use in the phantom row.
data I

-- | Creates an indexed HTML property.
prop
  :: forall value r i
   . IsProp value
  => PropName value
  -> Maybe AttrName
  -> value
  -> IProp r i
prop = (unsafeCoerce :: (PropName value -> Maybe AttrName -> value -> Prop i) -> PropName value -> Maybe AttrName -> value -> IProp r i) Core.prop

-- | Creates an indexed HTML attribute.
attr :: forall r i. AttrName -> String -> IProp r i
attr = (unsafeCoerce :: (AttrName -> String -> Prop i) -> AttrName -> String -> IProp r i) Core.attr

-- | The `ref` property allows an input to be raised once a `HTMLElement` has
-- | been created or destroyed in the DOM for the element that the property is
-- | attached to.
ref :: forall r i. (Maybe HTMLElement -> Maybe i) -> IProp (ref :: I | r) i
ref = (unsafeCoerce :: ((Maybe HTMLElement -> Maybe i) -> Prop i) -> (Maybe HTMLElement -> Maybe i) -> IProp (ref :: I | r) i) (lmap (lmap refToMaybe) Ref)
  where
  refToMaybe = case _ of
    Created x -> Just x
    Removed _ -> Nothing

alt :: forall r i. String -> IProp (alt :: I | r) i
alt = prop (PropName "alt") (Just $ AttrName "alt")

charset :: forall r i. String -> IProp (charset :: I | r) i
charset = prop (PropName "charset") (Just $ AttrName "charset")

class_ :: forall r i. ClassName -> IProp (class :: I | r) i
class_ = prop (PropName "className") (Just $ AttrName "class") <<< unwrap

classes :: forall r i. Array ClassName -> IProp (class :: I | r) i
classes = prop (PropName "className") (Just $ AttrName "class") <<< joinWith " " <<< map unwrap

cols :: forall r i. Int -> IProp (cols :: I | r) i
cols = prop (PropName "cols") (Just $ AttrName "cols")

rows :: forall r i. Int -> IProp (rows :: I | r) i
rows = prop (PropName "rows") (Just $ AttrName "rows")

colSpan :: forall r i. Int -> IProp (colSpan :: I | r) i
colSpan = prop (PropName "colSpan") (Just $ AttrName "colspan")

rowSpan :: forall r i. Int -> IProp (rowSpan :: I | r) i
rowSpan = prop (PropName "rowSpan") (Just $ AttrName "rowspan")

for :: forall r i. String -> IProp (for :: I | r) i
for = prop (PropName "htmlFor") (Just $ AttrName "for")

data LengthLiteral
  = Pixels Int
  | Percent Number

printLengthLiteral :: LengthLiteral -> String
printLengthLiteral = case _ of
  Pixels n -> show n
  Percent n -> show n <> "%"

height :: forall r i. LengthLiteral -> IProp (height :: I | r) i
height = prop (PropName "height") (Just $ AttrName "height") <<< printLengthLiteral

width :: forall r i. LengthLiteral -> IProp (width :: I | r) i
width = prop (PropName "width") (Just $ AttrName "width") <<< printLengthLiteral

href :: forall r i. String -> IProp (href :: I | r) i
href = prop (PropName "href") (Just $ AttrName "href")

id_ :: forall r i. String -> IProp (id :: I | r) i
id_ = prop (PropName "id") (Just $ AttrName "id")

name :: forall r i. String -> IProp (name :: I | r) i
name = prop (PropName "name") (Just $ AttrName "name")

rel :: forall r i. String -> IProp (rel :: I | r) i
rel = prop (PropName "rel") (Just $ AttrName "rel")

src :: forall r i. String -> IProp (src :: I | r) i
src = prop (PropName "src") (Just $ AttrName "src")

target :: forall r i. String -> IProp (target :: I | r) i
target = prop (PropName "target") (Just $ AttrName "target")

title :: forall r i. String -> IProp (title :: I | r) i
title = prop (PropName "title") (Just $ AttrName "title")

data FormMethod
  = POST
  | GET

renderFormMethod :: FormMethod -> String
renderFormMethod = case _ of
  POST -> "post"
  GET -> "get"

method :: forall r i. FormMethod -> IProp (method :: I | r) i
method = prop (PropName "method") (Just $ AttrName "method") <<< renderFormMethod

action :: forall r i. String -> IProp (action :: I | r) i
action = prop (PropName "action") (Just $ AttrName "action")

enctype :: forall r i. MediaType -> IProp (action :: I | r) i
enctype = prop (PropName "enctype") (Just $ AttrName "enctype") <<< unwrap

novalidate :: forall r i. Boolean -> IProp (action :: I | r) i
novalidate = prop (PropName "noValidate") (Just $ AttrName "novalidate")

data InputType
  = InputButton
  | InputCheckbox
  | InputColor
  | InputDate
  | InputDatetime
  | InputDatetimeLocal
  | InputEmail
  | InputFile
  | InputHidden
  | InputImage
  | InputMonth
  | InputNumber
  | InputPassword
  | InputRadio
  | InputRange
  | InputReset
  | InputSearch
  | InputSubmit
  | InputTel
  | InputText
  | InputTime
  | InputUrl
  | InputWeek

renderInputType :: InputType -> String
renderInputType = case _ of
  InputButton -> "button"
  InputCheckbox -> "checkbox"
  InputColor -> "color"
  InputDate -> "date"
  InputDatetime -> "datetime"
  InputDatetimeLocal -> "datetime-local"
  InputEmail -> "email"
  InputFile -> "file"
  InputHidden -> "hidden"
  InputImage -> "image"
  InputMonth -> "month"
  InputNumber -> "number"
  InputPassword -> "password"
  InputRadio -> "radio"
  InputRange -> "range"
  InputReset -> "reset"
  InputSearch -> "search"
  InputSubmit -> "submit"
  InputTel -> "tel"
  InputText -> "text"
  InputTime -> "time"
  InputUrl -> "url"
  InputWeek -> "week"

_type :: forall i r value. IsProp value => value -> IProp (r :: # *) i
_type = prop (PropName "type") (Just $ AttrName "type")

inputType :: forall r i. InputType -> IProp (inputType :: I | r) i
inputType = _type <<< renderInputType

data MenuType
  = MenuList
  | MenuContext
  | MenuToolbar

renderMenuType :: MenuType -> String
renderMenuType = case _ of
  MenuList -> "list"
  MenuContext -> "context"
  MenuToolbar -> "toolbar"

menuType :: forall r i. MenuType -> IProp (menuType :: I | r) i
menuType = _type <<< renderMenuType

data MenuitemType
  = MenuitemCommand
  | MenuitemCheckbox
  | MenuitemRadio

renderMenuitemType :: MenuitemType -> String
renderMenuitemType = case _ of
  MenuitemCommand -> "command"
  MenuitemCheckbox -> "checkbox"
  MenuitemRadio -> "radio"

menuitemType :: forall r i. MenuitemType -> IProp (menuitemType :: I | r) i
menuitemType= _type <<< renderMenuitemType

mediaType :: forall r i. MediaType -> IProp (mediaType :: I | r) i
mediaType = _type <<< unwrap

data ButtonType
  = ButtonButton
  | ButtonSubmit
  | ButtonReset

renderButtonType :: ButtonType -> String
renderButtonType = case _ of
  ButtonButton -> "button"
  ButtonSubmit -> "submit"
  ButtonReset -> "reset"

buttonType :: forall r i. ButtonType -> IProp (buttonType :: I | r) i
buttonType = _type <<< renderButtonType

data CaseType
  = Uppercase
  | Lowercase

data NumeralType
  = NumeralDecimal
  | NumeralRoman CaseType

data OrderedListType
  = OrderedListNumeric NumeralType
  | OrderedListAlphabetic CaseType

renderOrderedListType :: OrderedListType -> String
renderOrderedListType = case _ of
  OrderedListNumeric NumeralDecimal -> "1"
  OrderedListNumeric (NumeralRoman Lowercase) -> "i"
  OrderedListNumeric (NumeralRoman Uppercase) -> "I"
  OrderedListAlphabetic Lowercase -> "a"
  OrderedListAlphabetic Uppercase -> "A"

olType :: forall r i. OrderedListType -> IProp (olType :: I | r) i
olType = _type <<< renderOrderedListType

value :: forall r i. String -> IProp (value :: I | r) i
value = prop (PropName "value") (Just $ AttrName "value")

enabled :: forall r i. Boolean -> IProp (disabled :: I | r) i
enabled = disabled <<< not

disabled :: forall r i. Boolean -> IProp (disabled :: I | r) i
disabled = prop (PropName "disabled") (Just $ AttrName "disabled")

required :: forall r i. Boolean -> IProp (required :: I | r) i
required = prop (PropName "required") (Just $ AttrName "required")

readonly :: forall r i. Boolean -> IProp (readonly :: I | r) i
readonly = prop (PropName "readOnly") (Just $ AttrName "readonly")

spellcheck :: forall r i. Boolean -> IProp (spellcheck :: I | r) i
spellcheck = prop (PropName "spellcheck") (Just $ AttrName "spellcheck")

checked :: forall r i. Boolean -> IProp (checked :: I | r) i
checked = prop (PropName "checked") (Just $ AttrName "checked")

selected :: forall r i. Boolean -> IProp (selected :: I | r) i
selected = prop (PropName "selected") (Just $ AttrName "selected")

placeholder :: forall r i. String -> IProp (placeholder :: I | r) i
placeholder = prop (PropName "placeholder") (Just $ AttrName "placeholder")

autocomplete :: forall r i. Boolean -> IProp (autocomplete :: I | r) i
autocomplete = prop (PropName "autocomplete") (Just $ AttrName "autocomplete") <<< (\b -> if b then "on" else "off")

autofocus :: forall r i. Boolean -> IProp (autofocus :: I | r) i
autofocus = prop (PropName "autofocus") (Just $ AttrName "autofocus")

multiple :: forall r i. Boolean -> IProp (multiple :: I | r) i
multiple = prop (PropName "multiple") (Just $ AttrName "multiple")

draggable :: forall r i. Boolean -> IProp (draggable :: I | r) i
draggable = prop (PropName "draggable") (Just $ AttrName "draggable")

tabIndex :: forall r i. Int -> IProp (tabIndex :: I | r) i
tabIndex = prop (PropName "tabIndex") (Just $ AttrName "tabindex")

type GlobalAttributes r =
  ( id :: I
  , name :: I
  , title :: I
  , class :: I
  , style :: I
  , spellcheck :: I
  , key :: I
  , draggable :: I
  , tabIndex :: I
  , ref :: I
  | r
  )

type GlobalEvents r =
  ( onContextMenu :: I
  | r
  )

type MouseEvents r =
  ( onDoubleClick :: I
  , onClick :: I
  , onMouseDown :: I
  , onMouseEnter :: I
  , onMouseLeave :: I
  , onMouseMove :: I
  , onMouseOver :: I
  , onMouseOut :: I
  , onMouseUp :: I
  | r
  )

type DragEvents r =
  ( onDrag :: I
  , onDragEnd :: I
  , onDragExit :: I
  , onDragEnter :: I
  , onDragLeave :: I
  , onDragOver :: I
  , onDragStart :: I
  , onDrop :: I
  | r
  )

type TouchEvents r =
  ( onTouchCancel :: I
  , onTouchEnd :: I
  , onTouchEnter :: I
  , onTouchLeave :: I
  , onTouchMove :: I
  , onTouchStart :: I
  | r
  )

type PointerEvents r =
  ( onPointerOver :: I
  , onPointerEnter :: I
  , onPointerDown :: I
  , onPointerMove :: I
  , onPointerUp :: I
  , onPointerCancel :: I
  , onPointerOut :: I
  , onPointerLeave :: I
  , gotPointerCapture :: I
  , lostPointerCapture :: I
  | r
  )

type KeyEvents r =
  ( onKeyDown :: I
  , onKeyUp :: I
  , onKeyPress :: I
  | r
  )

type TransitionEvents r =
  ( onTransitionEnd :: I
  | r
  )

type FocusEvents r =
  ( onBlur :: I
  , onFocus :: I
  , onFocusIn :: I
  , onFocusOut :: I
  | r
  )

type InteractiveEvents r = FocusEvents (TransitionEvents (KeyEvents (PointerEvents (TouchEvents (DragEvents (MouseEvents r))))))
type GlobalProperties r = GlobalAttributes (GlobalEvents r)
