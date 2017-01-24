-- | A closed signature of type-indexed (refined) HTML properties; these can be
-- | used to ensure correctness by construction, and then erased into the
-- | standard unrefined versions.
module Halogen.HTML.Properties
  ( IndexedProp(..)
  , IProp
  , I
  , prop
  , attr
  , ref

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

import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType)
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)

import DOM.Node.Types (Element)

import Halogen.HTML.Core (class IsProp, ClassName, AttrName(..), PropName(..), Prop)
import Halogen.HTML.Core as Core
import Halogen.Query.InputF (InputF(..))

import Unsafe.Coerce (unsafeCoerce)

-- | The phantom row `r` can be thought of as a context which is synthesized in
-- | the course of constructing a refined HTML expression.
newtype IndexedProp (r :: # *) i = IProp (Prop i)

derive instance newtypeIProp :: Newtype (IndexedProp r i) _

type IProp r p i = IndexedProp r (InputF p Unit i)

-- | A dummy type to use in the phantom row.
data I

-- | Creates an indexed HTML property.
prop
  :: forall value r p i
   . IsProp value
  => PropName value
  -> Maybe AttrName
  -> value
  -> IProp r p i
prop = (unsafeCoerce :: (PropName value -> Maybe AttrName -> value -> Prop (InputF p Unit i)) -> PropName value -> Maybe AttrName -> value -> IProp r p i) Core.prop

-- | Creates an indexed HTML attribute.
attr :: forall r p i. AttrName -> String -> IProp r p i
attr = (unsafeCoerce :: (AttrName -> String -> Prop (InputF p Unit i)) -> AttrName -> String -> IProp r p i) Core.attr

-- | The `ref` property allows an input to be raised once a `HTMLElement` has
-- | been created or destroyed in the DOM for the element that the property is
-- | attached to.
ref :: forall r p i. p -> IProp r p i
ref = (unsafeCoerce :: ((Maybe Element -> Maybe (InputF p Unit i)) -> Prop (InputF p Unit i)) -> (Maybe Element -> Maybe (InputF p Unit i)) -> IProp r p i) Core.ref <<< go
  where
  go :: p -> Maybe Element -> Maybe (InputF p Unit i)
  go p mel = Just $ RefUpdate p (toForeign <$> mel) unit

alt :: forall r p i. String -> IProp (alt :: I | r) p i
alt = prop (PropName "alt") (Just $ AttrName "alt")

charset :: forall r p i. String -> IProp (charset :: I | r) p i
charset = prop (PropName "charset") (Just $ AttrName "charset")

class_ :: forall r p i. ClassName -> IProp (class :: I | r) p i
class_ = prop (PropName "className") (Just $ AttrName "class") <<< unwrap

classes :: forall r p i. Array ClassName -> IProp (class :: I | r) p i
classes = prop (PropName "className") (Just $ AttrName "class") <<< joinWith " " <<< map unwrap

cols :: forall r p i. Int -> IProp (cols :: I | r) p i
cols = prop (PropName "cols") (Just $ AttrName "cols")

rows :: forall r p i. Int -> IProp (rows :: I | r) p i
rows = prop (PropName "rows") (Just $ AttrName "rows")

colSpan :: forall r p i. Int -> IProp (colSpan :: I | r) p i
colSpan = prop (PropName "colSpan") (Just $ AttrName "colspan")

rowSpan :: forall r p i. Int -> IProp (rowSpan :: I | r) p i
rowSpan = prop (PropName "rowSpan") (Just $ AttrName "rowspan")

for :: forall r p i. String -> IProp (for :: I | r) p i
for = prop (PropName "htmlFor") (Just $ AttrName "for")

data LengthLiteral
  = Pixels Int
  | Percent Number

printLengthLiteral :: LengthLiteral -> String
printLengthLiteral = case _ of
  Pixels n -> show n
  Percent n -> show n <> "%"

height :: forall r p i. LengthLiteral -> IProp (height :: I | r) p i
height = prop (PropName "height") (Just $ AttrName "height") <<< printLengthLiteral

width :: forall r p i. LengthLiteral -> IProp (width :: I | r) p i
width = prop (PropName "width") (Just $ AttrName "width") <<< printLengthLiteral

href :: forall r p i. String -> IProp (href :: I | r) p i
href = prop (PropName "href") (Just $ AttrName "href")

id_ :: forall r p i. String -> IProp (id :: I | r) p i
id_ = prop (PropName "id") (Just $ AttrName "id")

name :: forall r p i. String -> IProp (name :: I | r) p i
name = prop (PropName "name") (Just $ AttrName "name")

rel :: forall r p i. String -> IProp (rel :: I | r) p i
rel = prop (PropName "rel") (Just $ AttrName "rel")

src :: forall r p i. String -> IProp (src :: I | r) p i
src = prop (PropName "src") (Just $ AttrName "src")

target :: forall r p i. String -> IProp (target :: I | r) p i
target = prop (PropName "target") (Just $ AttrName "target")

title :: forall r p i. String -> IProp (title :: I | r) p i
title = prop (PropName "title") (Just $ AttrName "title")

data FormMethod
  = POST
  | GET

renderFormMethod :: FormMethod -> String
renderFormMethod = case _ of
  POST -> "post"
  GET -> "get"

method :: forall r p i. FormMethod -> IProp (method :: I | r) p i
method = prop (PropName "method") (Just $ AttrName "method") <<< renderFormMethod

action :: forall r p i. String -> IProp (action :: I | r) p i
action = prop (PropName "action") (Just $ AttrName "action")

enctype :: forall r p i. MediaType -> IProp (action :: I | r) p i
enctype = prop (PropName "enctype") (Just $ AttrName "enctype") <<< unwrap

novalidate :: forall r p i. Boolean -> IProp (action :: I | r) p i
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

_type :: forall r p i value. IsProp value => value -> IProp (r :: # *) p i
_type = prop (PropName "type") (Just $ AttrName "type")

inputType :: forall r p i. InputType -> IProp (inputType :: I | r) p i
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

menuType :: forall r p i. MenuType -> IProp (menuType :: I | r) p i
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

menuitemType :: forall r p i. MenuitemType -> IProp (menuitemType :: I | r) p i
menuitemType= _type <<< renderMenuitemType

mediaType :: forall r p i. MediaType -> IProp (mediaType :: I | r) p i
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

buttonType :: forall r p i. ButtonType -> IProp (buttonType :: I | r) p i
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

olType :: forall r p i. OrderedListType -> IProp (olType :: I | r) p i
olType = _type <<< renderOrderedListType

value :: forall r p i. String -> IProp (value :: I | r) p i
value = prop (PropName "value") (Just $ AttrName "value")

enabled :: forall r p i. Boolean -> IProp (disabled :: I | r) p i
enabled = disabled <<< not

disabled :: forall r p i. Boolean -> IProp (disabled :: I | r) p i
disabled = prop (PropName "disabled") (Just $ AttrName "disabled")

required :: forall r p i. Boolean -> IProp (required :: I | r) p i
required = prop (PropName "required") (Just $ AttrName "required")

readonly :: forall r p i. Boolean -> IProp (readonly :: I | r) p i
readonly = prop (PropName "readOnly") (Just $ AttrName "readonly")

spellcheck :: forall r p i. Boolean -> IProp (spellcheck :: I | r) p i
spellcheck = prop (PropName "spellcheck") (Just $ AttrName "spellcheck")

checked :: forall r p i. Boolean -> IProp (checked :: I | r) p i
checked = prop (PropName "checked") (Just $ AttrName "checked")

selected :: forall r p i. Boolean -> IProp (selected :: I | r) p i
selected = prop (PropName "selected") (Just $ AttrName "selected")

placeholder :: forall r p i. String -> IProp (placeholder :: I | r) p i
placeholder = prop (PropName "placeholder") (Just $ AttrName "placeholder")

autocomplete :: forall r p i. Boolean -> IProp (autocomplete :: I | r) p i
autocomplete = prop (PropName "autocomplete") (Just $ AttrName "autocomplete") <<< (\b -> if b then "on" else "off")

autofocus :: forall r p i. Boolean -> IProp (autofocus :: I | r) p i
autofocus = prop (PropName "autofocus") (Just $ AttrName "autofocus")

multiple :: forall r p i. Boolean -> IProp (multiple :: I | r) p i
multiple = prop (PropName "multiple") (Just $ AttrName "multiple")

draggable :: forall r p i. Boolean -> IProp (draggable :: I | r) p i
draggable = prop (PropName "draggable") (Just $ AttrName "draggable")

tabIndex :: forall r p i. Int -> IProp (tabIndex :: I | r) p i
tabIndex = prop (PropName "tabIndex") (Just $ AttrName "tabindex")

type GlobalAttributes r =
  ( id :: I
  , title :: I
  , class :: I
  , style :: I
  , spellcheck :: I
  , draggable :: I
  , lang :: I
  , translate :: I
  , dir :: I
  , hidden :: I
  , tabIndex :: I
  , accessKey :: I
  , contentEditable :: I
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
