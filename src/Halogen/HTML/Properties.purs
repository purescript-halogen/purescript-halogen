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

type IProp r f p = IndexedProp r (InputF p f Unit)

-- | A dummy type to use in the phantom row.
data I

-- | Creates an indexed HTML property.
prop
  :: forall value r f p
   . IsProp value
  => PropName value
  -> Maybe AttrName
  -> value
  -> IProp r f p
prop = (unsafeCoerce :: (PropName value -> Maybe AttrName -> value -> Prop (InputF p f Unit)) -> PropName value -> Maybe AttrName -> value -> IProp r f p) Core.prop

-- | Creates an indexed HTML attribute.
attr :: forall r f p. AttrName -> String -> IProp r f p
attr = (unsafeCoerce :: (AttrName -> String -> Prop (InputF p f Unit)) -> AttrName -> String -> IProp r f p) Core.attr

-- | The `ref` property allows an input to be raised once a `HTMLElement` has
-- | been created or destroyed in the DOM for the element that the property is
-- | attached to.
ref :: forall f p r. p -> IProp (ref :: I | r) f p
ref = (unsafeCoerce :: ((Maybe Element -> Maybe (InputF p f Unit)) -> Prop (InputF p f Unit)) -> (Maybe Element -> Maybe (InputF p f Unit)) -> IProp (ref :: I | r) f p) Core.ref <<< go
  where
  go :: p -> Maybe Element -> Maybe (InputF p f Unit)
  go p mel = Just $ RefUpdate p (toForeign <$> mel) unit

alt :: forall r f p. String -> IProp (alt :: I | r) f p
alt = prop (PropName "alt") (Just $ AttrName "alt")

charset :: forall r f p. String -> IProp (charset :: I | r) f p
charset = prop (PropName "charset") (Just $ AttrName "charset")

class_ :: forall r f p. ClassName -> IProp (class :: I | r) f p
class_ = prop (PropName "className") (Just $ AttrName "class") <<< unwrap

classes :: forall r f p. Array ClassName -> IProp (class :: I | r) f p
classes = prop (PropName "className") (Just $ AttrName "class") <<< joinWith " " <<< map unwrap

cols :: forall r f p. Int -> IProp (cols :: I | r) f p
cols = prop (PropName "cols") (Just $ AttrName "cols")

rows :: forall r f p. Int -> IProp (rows :: I | r) f p
rows = prop (PropName "rows") (Just $ AttrName "rows")

colSpan :: forall r f p. Int -> IProp (colSpan :: I | r) f p
colSpan = prop (PropName "colSpan") (Just $ AttrName "colspan")

rowSpan :: forall r f p. Int -> IProp (rowSpan :: I | r) f p
rowSpan = prop (PropName "rowSpan") (Just $ AttrName "rowspan")

for :: forall r f p. String -> IProp (for :: I | r) f p
for = prop (PropName "htmlFor") (Just $ AttrName "for")

data LengthLiteral
  = Pixels Int
  | Percent Number

printLengthLiteral :: LengthLiteral -> String
printLengthLiteral = case _ of
  Pixels n -> show n
  Percent n -> show n <> "%"

height :: forall r f p. LengthLiteral -> IProp (height :: I | r) f p
height = prop (PropName "height") (Just $ AttrName "height") <<< printLengthLiteral

width :: forall r f p. LengthLiteral -> IProp (width :: I | r) f p
width = prop (PropName "width") (Just $ AttrName "width") <<< printLengthLiteral

href :: forall r f p. String -> IProp (href :: I | r) f p
href = prop (PropName "href") (Just $ AttrName "href")

id_ :: forall r f p. String -> IProp (id :: I | r) f p
id_ = prop (PropName "id") (Just $ AttrName "id")

name :: forall r f p. String -> IProp (name :: I | r) f p
name = prop (PropName "name") (Just $ AttrName "name")

rel :: forall r f p. String -> IProp (rel :: I | r) f p
rel = prop (PropName "rel") (Just $ AttrName "rel")

src :: forall r f p. String -> IProp (src :: I | r) f p
src = prop (PropName "src") (Just $ AttrName "src")

target :: forall r f p. String -> IProp (target :: I | r) f p
target = prop (PropName "target") (Just $ AttrName "target")

title :: forall r f p. String -> IProp (title :: I | r) f p
title = prop (PropName "title") (Just $ AttrName "title")

data FormMethod
  = POST
  | GET

renderFormMethod :: FormMethod -> String
renderFormMethod = case _ of
  POST -> "post"
  GET -> "get"

method :: forall r f p. FormMethod -> IProp (method :: I | r) f p
method = prop (PropName "method") (Just $ AttrName "method") <<< renderFormMethod

action :: forall r f p. String -> IProp (action :: I | r) f p
action = prop (PropName "action") (Just $ AttrName "action")

enctype :: forall r f p. MediaType -> IProp (action :: I | r) f p
enctype = prop (PropName "enctype") (Just $ AttrName "enctype") <<< unwrap

novalidate :: forall r f p. Boolean -> IProp (action :: I | r) f p
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

_type :: forall r f p value. IsProp value => value -> IProp (r :: # *) f p
_type = prop (PropName "type") (Just $ AttrName "type")

inputType :: forall r f p. InputType -> IProp (inputType :: I | r) f p
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

menuType :: forall r f p. MenuType -> IProp (menuType :: I | r) f p
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

menuitemType :: forall r f p. MenuitemType -> IProp (menuitemType :: I | r) f p
menuitemType= _type <<< renderMenuitemType

mediaType :: forall r f p. MediaType -> IProp (mediaType :: I | r) f p
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

buttonType :: forall r f p. ButtonType -> IProp (buttonType :: I | r) f p
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

olType :: forall r f p. OrderedListType -> IProp (olType :: I | r) f p
olType = _type <<< renderOrderedListType

value :: forall r f p. String -> IProp (value :: I | r) f p
value = prop (PropName "value") (Just $ AttrName "value")

enabled :: forall r f p. Boolean -> IProp (disabled :: I | r) f p
enabled = disabled <<< not

disabled :: forall r f p. Boolean -> IProp (disabled :: I | r) f p
disabled = prop (PropName "disabled") (Just $ AttrName "disabled")

required :: forall r f p. Boolean -> IProp (required :: I | r) f p
required = prop (PropName "required") (Just $ AttrName "required")

readonly :: forall r f p. Boolean -> IProp (readonly :: I | r) f p
readonly = prop (PropName "readOnly") (Just $ AttrName "readonly")

spellcheck :: forall r f p. Boolean -> IProp (spellcheck :: I | r) f p
spellcheck = prop (PropName "spellcheck") (Just $ AttrName "spellcheck")

checked :: forall r f p. Boolean -> IProp (checked :: I | r) f p
checked = prop (PropName "checked") (Just $ AttrName "checked")

selected :: forall r f p. Boolean -> IProp (selected :: I | r) f p
selected = prop (PropName "selected") (Just $ AttrName "selected")

placeholder :: forall r f p. String -> IProp (placeholder :: I | r) f p
placeholder = prop (PropName "placeholder") (Just $ AttrName "placeholder")

autocomplete :: forall r f p. Boolean -> IProp (autocomplete :: I | r) f p
autocomplete = prop (PropName "autocomplete") (Just $ AttrName "autocomplete") <<< (\b -> if b then "on" else "off")

autofocus :: forall r f p. Boolean -> IProp (autofocus :: I | r) f p
autofocus = prop (PropName "autofocus") (Just $ AttrName "autofocus")

multiple :: forall r f p. Boolean -> IProp (multiple :: I | r) f p
multiple = prop (PropName "multiple") (Just $ AttrName "multiple")

draggable :: forall r f p. Boolean -> IProp (draggable :: I | r) f p
draggable = prop (PropName "draggable") (Just $ AttrName "draggable")

tabIndex :: forall r f p. Int -> IProp (tabIndex :: I | r) f p
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
