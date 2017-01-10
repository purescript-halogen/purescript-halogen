module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Foldable (traverse_)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild, parentNode, replaceChild)
import DOM.Node.Types (Document, Element, Node) as DOM

import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component, ComponentSlot)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP

type VHTML f g p eff =
  V.VDom (Array (Prop (f Unit))) (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))

newtype RenderState s f g p o eff =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (Eff (HalogenEffects eff)) (VHTML f g p eff) DOM.Node
    }

mkSpec
  :: forall f g p eff
   . (f Unit -> Eff (HalogenEffects eff) Unit)
  -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX HTML RenderState g eff))
  -> DOM.Document
  -> V.VDomSpec
      (HalogenEffects eff)
      (Array (VP.Prop (f Unit)))
      (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
mkSpec handler renderChild document =
  V.VDomSpec { buildWidget, buildAttributes, document }
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
    rsx <- renderChild slot
    let node = unRenderStateX (\(RenderState { node }) -> node) rsx
    pure (V.Step node patch done)

  patch
    :: V.VDomMachine (HalogenEffects eff)
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
          DOM.Node
  patch slot = do
    rsx <- renderChild slot
    let node = unRenderStateX (\(RenderState { node }) -> node) rsx
    pure (V.Step node patch done)

  done :: Eff (HalogenEffects eff) Unit
  done = pure unit

runUI
  :: forall f eff o
   . Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI component element = do
  document <- liftEff $ DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component

renderSpec
  :: forall eff
   . DOM.Document
  -> HTMLElement
  -> AD.RenderSpec HTML RenderState eff
renderSpec document container = { render, renderChild: id }
  where

  render
    :: forall s f g p o
     . (forall x. f x -> Eff (HalogenEffects eff) Unit)
    -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX HTML RenderState g eff))
    -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
    -> Maybe (RenderState s f g p o eff)
    -> Eff (HalogenEffects eff) (RenderState s f g p o eff)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        let spec = mkSpec handler child document
        machine <- V.buildVDom spec vdom
        let node = V.extract machine
        appendChild node (htmlElementToNode container)
        pure $ RenderState { machine, node }
      Just (RenderState { machine, node }) -> do
        machine' <- V.step machine vdom
        let newNode = V.extract machine'
        when (not nodeRefEq node newNode) (substInParent node newNode)
        pure $ RenderState { machine: machine', node: newNode }

substInParent :: forall eff. DOM.Node -> DOM.Node -> Eff (dom :: DOM | eff) Unit
substInParent oldNode newNode = do
  npn <- parentNode oldNode
  traverse_ (\pn -> replaceChild newNode oldNode pn) (toMaybe npn)

foreign import nodeRefEq :: DOM.Node -> DOM.Node -> Boolean
