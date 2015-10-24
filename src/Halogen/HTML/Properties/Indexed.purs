-- | A closed signature of type-indexed (refined) HTML properties; these can be
-- | used to ensure correctness by construction, and then erased into the standard
-- | unrefined versions.
module Halogen.HTML.Properties.Indexed
  ( IProp()
  , I()

  , ButtonType(..)
  , InputType(..)
  , MediaType(..)
  , MenuType(..)
  , MenuitemType(..)
  , OrderedListType(..)
  , NumeralType(..)
  , CaseType(..)

  , key
  , alt
  , charset
  , class_, classes
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

  -- replacements for `type`
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

  , initializer
  , finalizer

  , module PExport

  , GlobalAttributes()
  , GlobalEvents()
  , MouseEvents()
  , KeyEvents()
  , FocusEvents()
  , InteractiveEvents()
  , GlobalProperties()
  ) where

import Prelude

import Data.Foldable
import Data.Tuple
import qualified Data.Array as A

import Unsafe.Coerce (unsafeCoerce)

import DOM.HTML.Types (HTMLElement())

import Halogen.HTML.Core (Prop(..), ClassName(), prop, propName, attrName, runClassName)
import qualified Halogen.HTML.Properties (LengthLiteral(..)) as PExport
import qualified Halogen.HTML.Properties as P

-- | The phantom row `r` can be thought of as a context which is synthesized in the
-- | course of constructing a refined HTML expression.
newtype IProp (r :: # *) i = IProp (Prop i)

-- | A dummy type to use in the phantom row.
data I

key :: forall r i. String -> IProp (key :: I | r) i
key = unsafeCoerce P.key

alt :: forall r i. String -> IProp (alt :: I | r) i
alt = unsafeCoerce P.alt

charset :: forall r i. String -> IProp (charset :: I | r) i
charset = unsafeCoerce P.charset

class_ :: forall r i. ClassName -> IProp (class :: I | r) i
class_ = unsafeCoerce P.class_

classes :: forall r i. Array ClassName -> IProp (class :: I | r) i
classes = unsafeCoerce P.classes

colSpan :: forall r i. Int -> IProp (colSpan :: I | r) i
colSpan = unsafeCoerce P.colSpan

rowSpan :: forall r i. Int -> IProp (rowSpan :: I | r) i
rowSpan = unsafeCoerce P.rowSpan

for :: forall r i. String -> IProp (for :: I | r) i
for = unsafeCoerce P.for

height :: forall r i. P.LengthLiteral -> IProp (height :: I | r) i
height = unsafeCoerce P.height

width :: forall r i. P.LengthLiteral -> IProp (width :: I | r) i
width = unsafeCoerce P.width

href :: forall r i. String -> IProp (href :: I | r) i
href = unsafeCoerce P.href

id_ :: forall r i. String -> IProp (id :: I | r) i
id_ = unsafeCoerce P.id_

name :: forall r i. String -> IProp (name :: I | r) i
name = unsafeCoerce P.name

rel :: forall r i. String -> IProp (rel :: I | r) i
rel = unsafeCoerce P.rel

src :: forall r i. String -> IProp (src :: I | r) i
src = unsafeCoerce P.src

target :: forall r i. String -> IProp (target :: I | r) i
target = unsafeCoerce P.target

title :: forall r i. String -> IProp (title :: I | r) i
title = unsafeCoerce P.title

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
renderInputType ty =
  case ty of
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

inputType :: forall r i. InputType -> IProp (inputType :: I | r) i
inputType = unsafeCoerce P.type_ <<< renderInputType

data MenuType
  = MenuList
  | MenuContext
  | MenuToolbar

renderMenuType :: MenuType -> String
renderMenuType ty =
  case ty of
    MenuList -> "list"
    MenuContext -> "context"
    MenuToolbar -> "toolbar"

menuType :: forall r i. MenuType -> IProp (menuType :: I | r) i
menuType = unsafeCoerce P.type_ <<< renderMenuType

data MenuitemType
  = MenuitemCommand
  | MenuitemCheckbox
  | MenuitemRadio

renderMenuitemType :: MenuitemType -> String
renderMenuitemType ty =
  case ty of
    MenuitemCommand -> "command"
    MenuitemCheckbox -> "checkbox"
    MenuitemRadio -> "radio"

menuitemType :: forall r i. MenuitemType -> IProp (menuitemType :: I | r) i
menuitemType= unsafeCoerce P.type_ <<< renderMenuitemType

type MediaType =
  { type :: String
  , subtype :: String
  , parameters :: Array (Tuple String String)
  }

renderMediaType :: MediaType -> String
renderMediaType ty = ty.type ++ "/" ++ ty.subtype ++ renderParameters ty.parameters
  where
    renderParameters :: Array (Tuple String String) -> String
    renderParameters ps
      | A.length ps == 0 = ""
      | otherwise = ";" ++ intercalate ";" (renderParameter <$> ps)

    renderParameter :: Tuple String String -> String
    renderParameter (Tuple k v) = k ++ "=" ++ v

mediaType :: forall r i. MediaType -> IProp (mediaType :: I | r) i
mediaType = unsafeCoerce P.type_ <<< renderMediaType

data ButtonType
  = ButtonButton
  | ButtonSubmit
  | ButtonReset

renderButtonType :: ButtonType -> String
renderButtonType ty =
  case ty of
    ButtonButton -> "button"
    ButtonSubmit -> "submit"
    ButtonReset -> "reset"

buttonType :: forall r i. ButtonType -> IProp (buttonType :: I | r) i
buttonType = unsafeCoerce P.type_ <<< renderButtonType

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
renderOrderedListType ty =
  case ty of
    OrderedListNumeric nty ->
      case nty of
        NumeralDecimal -> "1"
        NumeralRoman cty ->
          case cty of
            Lowercase -> "i"
            Uppercase -> "I"
    OrderedListAlphabetic cty ->
      case cty of
        Lowercase -> "a"
        Uppercase -> "A"

olType :: forall r i. OrderedListType -> IProp (olType :: I | r) i
olType = unsafeCoerce P.type_ <<< renderOrderedListType

value :: forall r i. String -> IProp (value :: I | r) i
value = unsafeCoerce P.value

disabled :: forall r i. Boolean -> IProp (disabled :: I | r) i
disabled = unsafeCoerce P.disabled

required :: forall r i. Boolean -> IProp (required :: I | r) i
required = unsafeCoerce P.required

readonly :: forall r i. Boolean -> IProp (readonly :: I | r) i
readonly = unsafeCoerce P.readonly

spellcheck :: forall r i. Boolean -> IProp (spellcheck :: I | r) i
spellcheck = unsafeCoerce P.spellcheck

enabled :: forall r i. Boolean -> IProp (disabled :: I | r) i
enabled = disabled <<< not

checked :: forall r i. Boolean -> IProp (checked :: I | r) i
checked = unsafeCoerce P.checked

selected :: forall r i. Boolean -> IProp (selected :: I | r) i
selected = unsafeCoerce P.selected

placeholder :: forall r i. String -> IProp (placeholder :: I | r) i
placeholder = unsafeCoerce P.placeholder

initializer :: forall r i. (HTMLElement -> i) -> IProp (initializer :: I | r) i
initializer = unsafeCoerce P.initializer

finalizer :: forall r i. (HTMLElement -> i) -> IProp (finalizer :: I | r) i
finalizer = unsafeCoerce P.finalizer

type GlobalAttributes r =
  ( id :: I
  , name :: I
  , title :: I
  , class :: I
  , spellcheck :: I
  , key :: I
  , initializer :: I
  , finalizer :: I
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

type KeyEvents r =
  ( onKeyDown :: I
  , onKeyUp :: I
  , onKeyPress :: I
  | r
  )

type FocusEvents r =
  ( onBlur :: I
  , onFocus :: I
  , onFocusIn :: I
  , onFocusOut :: I
  | r
  )

type InteractiveEvents r = FocusEvents (KeyEvents (MouseEvents r))
type GlobalProperties r = GlobalAttributes (GlobalEvents r)
