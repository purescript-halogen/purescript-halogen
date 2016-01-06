module Halogen.HTML.Properties.Indexed.ARIA
  ( ariaAtomic
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
  , role
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

import Halogen.HTML.Properties.Indexed (IProp())
import qualified Halogen.HTML.Properties.ARIA as P

ariaActivedescendant :: forall r i. String -> IProp r i
ariaActivedescendant = unsafeCoerce P.ariaActivedescendant

ariaAtomic :: forall r i. String -> IProp r i
ariaAtomic = unsafeCoerce P.ariaAtomic

ariaAutocomplete :: forall r i. String -> IProp r i
ariaAutocomplete = unsafeCoerce P.ariaAutocomplete

ariaBusy :: forall r i. String -> IProp r i
ariaBusy = unsafeCoerce P.ariaBusy

ariaChecked :: forall r i. String -> IProp r i
ariaChecked = unsafeCoerce P.ariaChecked

ariaControls :: forall r i. String -> IProp r i
ariaControls = unsafeCoerce P.ariaControls

ariaDescribedby :: forall r i. String -> IProp r i
ariaDescribedby = unsafeCoerce P.ariaDescribedby

ariaDisabled :: forall r i. String -> IProp r i
ariaDisabled = unsafeCoerce P.ariaDisabled

ariaDropeffect :: forall r i. String -> IProp r i
ariaDropeffect = unsafeCoerce P.ariaDropeffect

ariaExpanded :: forall r i. String -> IProp r i
ariaExpanded = unsafeCoerce P.ariaExpanded

ariaFlowto :: forall r i. String -> IProp r i
ariaFlowto = unsafeCoerce P.ariaFlowto

ariaGrabbed :: forall r i. String -> IProp r i
ariaGrabbed = unsafeCoerce P.ariaGrabbed

ariaHaspopup :: forall r i. String -> IProp r i
ariaHaspopup = unsafeCoerce P.ariaHaspopup

ariaHidden :: forall r i. String -> IProp r i
ariaHidden = unsafeCoerce P.ariaHidden

ariaInvalid :: forall r i. String -> IProp r i
ariaInvalid = unsafeCoerce P.ariaInvalid

ariaLabel :: forall r i. String -> IProp r i
ariaLabel = unsafeCoerce P.ariaLabel

ariaLabelledby :: forall r i. String -> IProp r i
ariaLabelledby = unsafeCoerce P.ariaLabelledby

ariaLevel :: forall r i. String -> IProp r i
ariaLevel = unsafeCoerce P.ariaLevel

ariaLive :: forall r i. String -> IProp r i
ariaLive = unsafeCoerce P.ariaLive

ariaMultiline :: forall r i. String -> IProp r i
ariaMultiline = unsafeCoerce P.ariaMultiline

ariaMultiselectable :: forall r i. String -> IProp r i
ariaMultiselectable = unsafeCoerce P.ariaMultiselectable

ariaOrientation :: forall r i. String -> IProp r i
ariaOrientation = unsafeCoerce P.ariaOrientation

ariaOwns :: forall r i. String -> IProp r i
ariaOwns = unsafeCoerce P.ariaOwns

ariaPosinset :: forall r i. String -> IProp r i
ariaPosinset = unsafeCoerce P.ariaPosinset

ariaPressed :: forall r i. String -> IProp r i
ariaPressed = unsafeCoerce P.ariaPressed

ariaReadonly :: forall r i. String -> IProp r i
ariaReadonly = unsafeCoerce P.ariaReadonly

ariaRelevant :: forall r i. String -> IProp r i
ariaRelevant = unsafeCoerce P.ariaRelevant

ariaRequired :: forall r i. String -> IProp r i
ariaRequired = unsafeCoerce P.ariaRequired

ariaSelected :: forall r i. String -> IProp r i
ariaSelected = unsafeCoerce P.ariaSelected

ariaSetsize :: forall r i. String -> IProp r i
ariaSetsize = unsafeCoerce P.ariaSetsize

ariaSort :: forall r i. String -> IProp r i
ariaSort = unsafeCoerce P.ariaSort

ariaValuemax :: forall r i. String -> IProp r i
ariaValuemax = unsafeCoerce P.ariaValuemax

ariaValuemin :: forall r i. String -> IProp r i
ariaValuemin = unsafeCoerce P.ariaValuemin

ariaValuenow :: forall r i. String -> IProp r i
ariaValuenow = unsafeCoerce P.ariaValuenow

ariaValuetext :: forall r i. String -> IProp r i
ariaValuetext = unsafeCoerce P.ariaValuetext

role :: forall r i. String -> IProp r i
role = unsafeCoerce P.role

