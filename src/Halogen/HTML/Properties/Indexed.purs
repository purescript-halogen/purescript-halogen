-- | A closed signature of type-indexed (refined) HTML properties; these can be
-- | used to ensure correctness by construction, and then erased into the standard
-- | unrefined versions.
module Halogen.HTML.Properties.Indexed
  ( IProp(..)
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
  , autocomplete
  , autofocus

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

import Halogen.HTML.Core (Prop(), ClassName())
import qualified Halogen.HTML.Properties (LengthLiteral(..)) as PExport
import qualified Halogen.HTML.Properties as P

-- | The phantom row `r` can be thought of as a context which is synthesized in the
-- | course of constructing a refined HTML expression.
newtype IProp (r :: # *) i = IProp (Prop i)

-- | A dummy type to use in the phantom row.
data I

key :: forall r i. String -> IProp (key :: I | r) i
key = unsafeCoerce P.key

ariaActivedescendant :: forall r i. String -> IProp (ariaActivedescendant :: I | r) i
ariaActivedescendant = unsafeCoerce P.ariaActivedescendant

ariaAtomic :: forall r i. String -> IProp (ariaAtomic :: I | r) i
ariaAtomic = unsafeCoerce P.ariaAtomic

ariaAutocomplete :: forall r i. String -> IProp (ariaAutocomplete :: I | r) i
ariaAutocomplete = unsafeCoerce P.ariaAutocomplete

ariaBusy :: forall r i. String -> IProp (ariaBusy :: I | r) i
ariaBusy = unsafeCoerce P.ariaBusy

ariaChecked :: forall r i. String -> IProp (ariaChecked :: I | r) i
ariaChecked = unsafeCoerce P.ariaChecked

ariaControls :: forall r i. String -> IProp (ariaControls :: I | r) i
ariaControls = unsafeCoerce P.ariaControls

ariaDescribedby :: forall r i. String -> IProp (ariaDescribedby :: I | r) i
ariaDescribedby = unsafeCoerce P.ariaDescribedby

ariaDisabled :: forall r i. String -> IProp (ariaDisabled :: I | r) i
ariaDisabled = unsafeCoerce P.ariaDisabled

ariaDropeffect :: forall r i. String -> IProp (ariaDropeffect :: I | r) i
ariaDropeffect = unsafeCoerce P.ariaDropeffect

ariaExpanded :: forall r i. String -> IProp (ariaExpanded :: I | r) i
ariaExpanded = unsafeCoerce P.ariaExpanded

ariaFlowto :: forall r i. String -> IProp (ariaFlowto :: I | r) i
ariaFlowto = unsafeCoerce P.ariaFlowto

ariaGrabbed :: forall r i. String -> IProp (ariaGrabbed :: I | r) i
ariaGrabbed = unsafeCoerce P.ariaGrabbed

ariaHaspopup :: forall r i. String -> IProp (ariaHaspopup :: I | r) i
ariaHaspopup = unsafeCoerce P.ariaHaspopup

ariaHidden :: forall r i. String -> IProp (ariaHidden :: I | r) i
ariaHidden = unsafeCoerce P.ariaHidden

ariaInvalid :: forall r i. String -> IProp (ariaInvalid :: I | r) i
ariaInvalid = unsafeCoerce P.ariaInvalid

ariaLabel :: forall r i. String -> IProp (ariaLabel :: I | r) i
ariaLabel = unsafeCoerce P.ariaLabel

ariaLabelledby :: forall r i. String -> IProp (ariaLabelledby :: I | r) i
ariaLabelledby = unsafeCoerce P.ariaLabelledby

ariaLevel :: forall r i. String -> IProp (ariaLevel :: I | r) i
ariaLevel = unsafeCoerce P.ariaLevel

ariaLive :: forall r i. String -> IProp (ariaLive :: I | r) i
ariaLive = unsafeCoerce P.ariaLive

ariaMultiline :: forall r i. String -> IProp (ariaMultiline :: I | r) i
ariaMultiline = unsafeCoerce P.ariaMultiline

ariaMultiselectable :: forall r i. String -> IProp (ariaMultiselectable :: I | r) i
ariaMultiselectable = unsafeCoerce P.ariaMultiselectable

ariaOrientation :: forall r i. String -> IProp (ariaOrientation :: I | r) i
ariaOrientation = unsafeCoerce P.ariaOrientation

ariaOwns :: forall r i. String -> IProp (ariaOwns :: I | r) i
ariaOwns = unsafeCoerce P.ariaOwns

ariaPosinset :: forall r i. String -> IProp (ariaPosinset :: I | r) i
ariaPosinset = unsafeCoerce P.ariaPosinset

ariaPressed :: forall r i. String -> IProp (ariaPressed :: I | r) i
ariaPressed = unsafeCoerce P.ariaPressed

ariaReadonly :: forall r i. String -> IProp (ariaReadonly :: I | r) i
ariaReadonly = unsafeCoerce P.ariaReadonly

ariaRelevant :: forall r i. String -> IProp (ariaRelevant :: I | r) i
ariaRelevant = unsafeCoerce P.ariaRelevant

ariaRequired :: forall r i. String -> IProp (ariaRequired :: I | r) i
ariaRequired = unsafeCoerce P.ariaRequired

ariaSelected :: forall r i. String -> IProp (ariaSelected :: I | r) i
ariaSelected = unsafeCoerce P.ariaSelected

ariaSetsize :: forall r i. String -> IProp (ariaSetsize :: I | r) i
ariaSetsize = unsafeCoerce P.ariaSetsize

ariaSort :: forall r i. String -> IProp (ariaSort :: I | r) i
ariaSort = unsafeCoerce P.ariaSort

ariaValuemax :: forall r i. String -> IProp (ariaValuemax :: I | r) i
ariaValuemax = unsafeCoerce P.ariaValuemax

ariaValuemin :: forall r i. String -> IProp (ariaValuemin :: I | r) i
ariaValuemin = unsafeCoerce P.ariaValuemin

ariaValuenow :: forall r i. String -> IProp (ariaValuenow :: I | r) i
ariaValuenow = unsafeCoerce P.ariaValuenow

ariaValuetext :: forall r i. String -> IProp (ariaValuetext :: I | r) i
ariaValuetext = unsafeCoerce P.ariaValuetext

alt :: forall r i. String -> IProp (alt :: I | r) i
alt = unsafeCoerce P.alt

charset :: forall r i. String -> IProp (charset :: I | r) i
charset = unsafeCoerce P.charset

class_ :: forall r i. ClassName -> IProp (class :: I | r) i
class_ = unsafeCoerce P.class_

classes :: forall r i. Array ClassName -> IProp (class :: I | r) i
classes = unsafeCoerce P.classes

cols :: forall r i. Int -> IProp (cols :: I | r) i
cols = unsafeCoerce P.cols

rows :: forall r i. Int -> IProp (rows :: I | r) i
rows = unsafeCoerce P.rows

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

autocomplete :: forall r i. Boolean -> IProp (autocomplete :: I | r) i
autocomplete = unsafeCoerce P.autocomplete

autofocus :: forall r i. Boolean -> IProp (autofocus :: I | r) i
autofocus = unsafeCoerce P.autofocus

initializer :: forall r i. (HTMLElement -> i) -> IProp (initializer :: I | r) i
initializer = unsafeCoerce P.initializer

finalizer :: forall r i. (HTMLElement -> i) -> IProp (finalizer :: I | r) i
finalizer = unsafeCoerce P.finalizer

type GlobalAttributes r =
  ( id :: I
  , name :: I
  , title :: I
  , class :: I
  , style :: I
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
