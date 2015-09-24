-- | A closed signature of type-indexed (refined) HTML properties; these can be
-- | used to ensure correctness by construction, and then erased into the standard
-- | unrefined versions.
module Halogen.HTML.Properties.Indexed
  ( IProp()
  , erase

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
import Data.Tuple
import Data.Foldable
import qualified Data.Array as A
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Properties (LengthLiteral(..)) as PExport
import qualified Halogen.HTML.Properties.Indexed.Unsafe as Unsafe
import Halogen.HTML.Core (Prop(..), ClassName(), prop, propName, attrName, runClassName)

-- | The phantom row `ρ` can be thought of as a context which is synthesized in the
-- | course of constructing a refined HTML expression.
type IProp (ρ :: # *) i = Unsafe.IProp ρ i

-- | The refined property can be erased into a normal one.
erase :: forall ρ i. IProp ρ i -> Prop i
erase (Unsafe.IProp p) = p

refine :: forall ρ i. Prop i -> IProp ρ i
refine = Unsafe.IProp

-- | A dummy type to use in the phantom row.
data I

key :: forall ρ i. String -> IProp (key :: I | ρ) i
key = refine <<< P.key

alt :: forall ρ i. String -> IProp (key :: I | ρ) i
alt = refine <<< P.alt

charset :: forall ρ i. String -> IProp (charset :: I | ρ) i
charset = refine <<< P.charset

class_ :: forall ρ i. ClassName -> IProp (class :: I | ρ) i
class_ = refine <<< P.class_

classes :: forall ρ i. Array ClassName -> IProp (class :: I | ρ) i
classes = refine <<< P.classes

colSpan :: forall ρ i. Int -> IProp (colSpan :: I | ρ) i
colSpan = refine <<< P.colSpan

rowSpan :: forall ρ i. Int -> IProp (rowSpan :: I | ρ) i
rowSpan = refine <<< P.rowSpan

for :: forall ρ i. String -> IProp (for :: I | ρ) i
for = refine <<< P.for

height :: forall ρ i. P.LengthLiteral -> IProp (height :: I | ρ) i
height = refine <<< P.height

width :: forall ρ i. P.LengthLiteral -> IProp (width :: I | ρ) i
width = refine <<< P.width

href :: forall ρ i. String -> IProp (href :: I | ρ) i
href = refine <<< P.href

id_ :: forall ρ i. String -> IProp (id :: I | ρ) i
id_ = refine <<< P.id_

name :: forall ρ i. String -> IProp (name :: I | ρ) i
name = refine <<< P.name

rel :: forall ρ i. String -> IProp (rel :: I | ρ) i
rel = refine <<< P.rel

src :: forall ρ i. String -> IProp (src :: I | ρ) i
src = refine <<< P.src

target :: forall ρ i. String -> IProp (target :: I | ρ) i
target = refine <<< P.target

title :: forall ρ i. String -> IProp (title :: I | ρ) i
title = refine <<< P.title

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

inputType :: forall ρ i. InputType -> IProp (inputType :: I | ρ) i
inputType = refine <<< P.type_ <<< renderInputType

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

menuType :: forall ρ i. MenuType -> IProp (menuType :: I | ρ) i
menuType = refine <<< P.type_ <<< renderMenuType

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

menuitemType :: forall ρ i. MenuitemType -> IProp (menuitemType :: I | ρ) i
menuitemType= refine <<< P.type_ <<< renderMenuitemType

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

mediaType :: forall ρ i. MediaType -> IProp (mediaType :: I | ρ) i
mediaType = refine <<< P.type_ <<< renderMediaType

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

buttonType :: forall ρ i. ButtonType -> IProp (buttonType :: I | ρ) i
buttonType = refine <<< P.type_ <<< renderButtonType

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

olType :: forall ρ i. OrderedListType -> IProp (olType :: I | ρ) i
olType = refine <<< P.type_ <<< renderOrderedListType

value :: forall ρ i. String -> IProp (value :: I | ρ) i
value = refine <<< P.value

disabled :: forall ρ i. Boolean -> IProp (disabled :: I | ρ) i
disabled = refine <<< P.disabled

required :: forall ρ i. Boolean -> IProp (required :: I | ρ) i
required = refine <<< P.required

readonly :: forall ρ i. Boolean -> IProp (readonly :: I | ρ) i
readonly = refine <<< P.readonly

spellcheck :: forall ρ i. Boolean -> IProp (spellcheck :: I | ρ) i
spellcheck = refine <<< P.spellcheck

enabled :: forall ρ i. Boolean -> IProp (disabled :: I | ρ) i
enabled = disabled <<< not

checked :: forall ρ i. Boolean -> IProp (checked :: I | ρ) i
checked = refine <<< P.checked

selected :: forall ρ i. Boolean -> IProp (selected :: I | ρ) i
selected = refine <<< P.selected

placeholder :: forall ρ i. String -> IProp (placeholder :: I | ρ) i
placeholder = refine <<< P.placeholder

type GlobalAttributes ρ =
  ( id :: I
  , name :: I
  , title :: I
  , class :: I
  , spellcheck :: I
  | ρ
  )

type GlobalEvents ρ =
  ( onContextMenu :: I
  | ρ
  )

type MouseEvents ρ =
  ( onDoubleClick :: I
  , onClick :: I
  , onMouseDown :: I
  , onMouseEnter :: I
  , onMouseLeave :: I
  , onMouseMove :: I
  , onMouseOver :: I
  , onMouseOut :: I
  , onMouseUp :: I
  | ρ
  )

type KeyEvents ρ =
  ( onKeyDown :: I
  , onKeyUp :: I
  , onKeyPress :: I
  | ρ
  )

type FocusEvents ρ =
  ( onBlur :: I
  , onFocus :: I
  , onFocusIn :: I
  , onFocusOut :: I
  | ρ
  )

type InteractiveEvents ρ = FocusEvents (KeyEvents (MouseEvents ρ))
type GlobalProperties ρ = GlobalAttributes (GlobalEvents ρ)
