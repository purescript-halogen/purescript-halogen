-- | This module provides `Prop` values for WAI-ARIA attributes.
module Halogen.HTML.Properties.ARIA
  ( ariaActivedescendant
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
  , role
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.HTML.Core (Prop(..), attrName)

ariaActivedescendant :: forall i. String -> Prop i
ariaActivedescendant = Attr Nothing (attrName "aria-activedescendant")

ariaAtomic :: forall i. String -> Prop i
ariaAtomic = Attr Nothing (attrName "aria-atomic")

ariaAutocomplete :: forall i. String -> Prop i
ariaAutocomplete = Attr Nothing (attrName "aria-autocomplete")

ariaBusy :: forall i. String -> Prop i
ariaBusy = Attr Nothing (attrName "aria-busy")

ariaChecked :: forall i. String -> Prop i
ariaChecked = Attr Nothing (attrName "aria-checked")

ariaControls :: forall i. String -> Prop i
ariaControls = Attr Nothing (attrName "aria-controls")

ariaDescribedby :: forall i. String -> Prop i
ariaDescribedby = Attr Nothing (attrName "aria-describedby")

ariaDisabled :: forall i. String -> Prop i
ariaDisabled = Attr Nothing (attrName "aria-disabled")

ariaDropeffect :: forall i. String -> Prop i
ariaDropeffect = Attr Nothing (attrName "aria-dropeffect")

ariaExpanded :: forall i. String -> Prop i
ariaExpanded = Attr Nothing (attrName "aria-expanded")

ariaFlowto :: forall i. String -> Prop i
ariaFlowto = Attr Nothing (attrName "aria-flowto")

ariaGrabbed :: forall i. String -> Prop i
ariaGrabbed = Attr Nothing (attrName "aria-grabbed")

ariaHaspopup :: forall i. String -> Prop i
ariaHaspopup = Attr Nothing (attrName "aria-haspopup")

ariaHidden :: forall i. String -> Prop i
ariaHidden = Attr Nothing (attrName "aria-hidden")

ariaInvalid :: forall i. String -> Prop i
ariaInvalid = Attr Nothing (attrName "aria-invalid")

ariaLabel :: forall i. String -> Prop i
ariaLabel = Attr Nothing (attrName "aria-label")

ariaLabelledby :: forall i. String -> Prop i
ariaLabelledby = Attr Nothing (attrName "aria-labelledby")

ariaLevel :: forall i. String -> Prop i
ariaLevel = Attr Nothing (attrName "aria-level")

ariaLive :: forall i. String -> Prop i
ariaLive = Attr Nothing (attrName "aria-live")

ariaMultiline :: forall i. String -> Prop i
ariaMultiline = Attr Nothing (attrName "aria-multiline")

ariaMultiselectable :: forall i. String -> Prop i
ariaMultiselectable = Attr Nothing (attrName "aria-multiselectable")

ariaOrientation :: forall i. String -> Prop i
ariaOrientation = Attr Nothing (attrName "aria-orientation")

ariaOwns :: forall i. String -> Prop i
ariaOwns = Attr Nothing (attrName "aria-owns")

ariaPosinset :: forall i. String -> Prop i
ariaPosinset = Attr Nothing (attrName "aria-posinset")

ariaPressed :: forall i. String -> Prop i
ariaPressed = Attr Nothing (attrName "aria-pressed")

ariaReadonly :: forall i. String -> Prop i
ariaReadonly = Attr Nothing (attrName "aria-readonly")

ariaRelevant :: forall i. String -> Prop i
ariaRelevant = Attr Nothing (attrName "aria-relevant")

ariaRequired :: forall i. String -> Prop i
ariaRequired = Attr Nothing (attrName "aria-required")

ariaSelected :: forall i. String -> Prop i
ariaSelected = Attr Nothing (attrName "aria-selected")

ariaSetsize :: forall i. String -> Prop i
ariaSetsize = Attr Nothing (attrName "aria-setsize")

ariaSort :: forall i. String -> Prop i
ariaSort = Attr Nothing (attrName "aria-sort")

ariaValuemax :: forall i. String -> Prop i
ariaValuemax = Attr Nothing (attrName "aria-valuemax")

ariaValuemin :: forall i. String -> Prop i
ariaValuemin = Attr Nothing (attrName "aria-valuemin")

ariaValuenow :: forall i. String -> Prop i
ariaValuenow = Attr Nothing (attrName "aria-valuenow")

ariaValuetext :: forall i. String -> Prop i
ariaValuetext = Attr Nothing (attrName "aria-valuetext")

role :: forall i. String -> Prop i
role = Attr Nothing (attrName "role")

