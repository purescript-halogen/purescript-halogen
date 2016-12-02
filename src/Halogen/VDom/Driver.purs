module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (empty)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Document, Element, Node) as DOM

import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Component (Component, ComponentSlot)
import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP

import Unsafe.Coerce (unsafeCoerce)

type VHTML f g p eff =
  V.VDom (Array (Prop (f Unit))) (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))

type RenderState' f g p eff =
  V.Step (Eff (HalogenEffects eff)) (VHTML f g p eff) DOM.Node

foreign import data RenderState :: # ! -> *

mkSpec
  :: forall f g p eff
   . (f Unit -> Eff (HalogenEffects eff) Unit)
  -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderState' f g p eff))
  -> DOM.Document
  -> V.VDomSpec
      (HalogenEffects eff)
      (Array (VP.Prop (f Unit)))
      (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
mkSpec handler renderChild document = V.VDomSpec
  { buildWidget: buildWidget
  , buildAttributes: buildAttributes
  , document: document
  }
  where

  buildAttributes
    :: DOM.Element
    -> V.VDomMachine (HalogenEffects eff) (Array (VP.Prop (f Unit))) Unit
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec (HalogenEffects eff)
          (Array (VP.Prop (f Unit)))
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
    -> V.VDomMachine (HalogenEffects eff)
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
          DOM.Node
  buildWidget spec slot = do
    vdom <- renderChild slot
    pure $ map ?x vdom
    -- machine <- V.buildVDom spec vdom
    -- let node = V.extract machine
    -- pure (V.Step node (patch node) done)
    -- x <- renderChild slot
    -- pure $ ?z x

  patch
    :: DOM.Node
    -> V.VDomMachine (HalogenEffects eff)
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
          DOM.Node
  patch node x = pure (V.Step node (patch node) done)

  done :: Eff (HalogenEffects eff) Unit
  done = pure unit

runUI
  :: forall f eff o
   . DOM.Document
  -> Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI document component element = AD.runUI (renderSpec document element) component

renderSpec
  :: forall eff
   . DOM.Document
  -> HTMLElement
  -> AD.RenderSpec HTML (RenderState eff) eff
renderSpec document container =
  { render: coeR render
  , renderChild: coeRC renderChild
  }
  where

  render :: Render eff
  render handler child (HTML vdom) componentType lastRender = do
    liftEff case lastRender of
      Nothing -> do
        let spec = mkSpec handler child document
        machine <- V.buildVDom spec vdom
        let node = V.extract machine
        appendChild node (htmlElementToNode container)
        pure machine
      Just machine ->
        V.step machine vdom

  coeR :: Render eff -> Render' eff
  coeR = unsafeCoerce

  renderChild :: forall f g p. RenderChild (RenderState' f g p) eff
  renderChild keyId lastRender =
    liftEff case lastRender of
      Nothing ->
        V.buildVDom (mkSpec (const (pure unit)) (const empty) document) (V.Text "<child>")
      Just r ->
        pure r

  coeRC :: forall f g p. RenderChild (RenderState' f g p) eff -> RenderChild RenderState eff
  coeRC = unsafeCoerce

type Render eff
  = forall f g p
   . (forall x. f x -> Eff (HalogenEffects eff) Unit)
  -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderState' f g p eff))
  -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
  -> AD.ComponentType
  -> Maybe (RenderState' f g p eff)
  -> Eff (HalogenEffects eff) (RenderState' f g p eff)

type Render' eff
  = forall f g p
   . (forall x. f x -> Eff (HalogenEffects eff) Unit)
  -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderState eff))
  -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
  -> AD.ComponentType
  -> Maybe (RenderState eff)
  -> Eff (HalogenEffects eff) (RenderState eff)

type RenderChild rs eff = Int -> Maybe (rs eff) -> Eff (HalogenEffects eff) (rs eff)
