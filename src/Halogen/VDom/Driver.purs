module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, modifyRef)

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Data.Foreign (Foreign, toForeign)
import Data.Nullable (toMaybe)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToNode, htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild, removeChild, parentNode, replaceChild) as DOM
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
    , refs :: Ref (M.Map VP.RefLabel Foreign)
    }

mkSpec
  :: forall f g p eff
   . (f Unit -> Eff (HalogenEffects eff) Unit)
  -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX RenderState eff))
  -> Ref (M.Map VP.RefLabel Foreign)
  -> DOM.Document
  -> V.VDomSpec
      (HalogenEffects eff)
      (Array (VP.Prop (f Unit)))
      (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
mkSpec handler renderChild refs document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.VDomMachine (HalogenEffects eff) (Array (VP.Prop (f Unit))) Unit
  buildAttributes = VP.buildProp handler handleRef

  handleRef
    :: VP.RefLabel
    -> VP.ElemRef DOM.Element
    -> Eff (HalogenEffects eff) Unit
  handleRef label er =
    modifyRef refs $ flip M.alter label $ const case er of
      VP.Created el -> Just (toForeign el)
      VP.Removed el -> Nothing

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
  :: forall f eff i o
   . Component HTML f i o (Aff (HalogenEffects eff))
  -> i
  -> DOM.HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI component i element = do
  document <- liftEff $ DOM.htmlDocumentToDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component i

renderSpec
  :: forall eff
   . DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec HTML RenderState eff
renderSpec document container =
  { render
  , renderChild: id
  , removeChild
  , getRef
  }
  where

  render
    :: forall s f g p o
     . (forall x. f x -> Eff (HalogenEffects eff) Unit)
    -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX RenderState eff))
    -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
    -> Maybe (RenderState s f g p o eff)
    -> Eff (HalogenEffects eff) (RenderState s f g p o eff)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        refs <- newRef M.empty
        let spec = mkSpec handler child refs document
        machine <- V.buildVDom spec vdom
        let node = V.extract machine
        DOM.appendChild node (DOM.htmlElementToNode container)
        pure $ RenderState { machine, node, refs }
      Just (RenderState { machine, node, refs }) -> do
        machine' <- V.step machine vdom
        let newNode = V.extract machine'
        when (not nodeRefEq node newNode) (substInParent node newNode)
        pure $ RenderState { machine: machine', node: newNode, refs }

removeChild
  :: forall eff o p g f s. RenderState s f g p o eff
  -> Eff (HalogenEffects eff) Unit
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (\pn -> DOM.removeChild node pn) (toMaybe npn)

getRef
  :: forall s f g p o eff
   . VP.RefLabel
  -> RenderState s f g p o eff
  -> Eff (HalogenEffects eff) (Maybe Foreign)
getRef p (RenderState { refs }) = M.lookup p <$> readRef refs

substInParent :: forall eff. DOM.Node -> DOM.Node -> Eff (dom :: DOM | eff) Unit
substInParent oldNode newNode = do
  npn <- DOM.parentNode oldNode
  traverse_ (\pn -> DOM.replaceChild newNode oldNode pn) (toMaybe npn)

foreign import nodeRefEq :: DOM.Node -> DOM.Node -> Boolean
