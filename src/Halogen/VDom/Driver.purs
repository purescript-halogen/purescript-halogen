module Halogen.VDom.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Functor.Coproduct (Coproduct, right)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Component (Component, ComponentSlot)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.Query.InputF (InputF)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node, appendChild, removeChild, parentNode, nextSibling, insertBefore) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement) as DOM
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as DOM

type VHTML msg ps =
  V.VDom (Array (Prop (InputF Unit msg))) (ComponentSlot HTML ps Aff msg)

type ChildRenderer msg ps
  = ComponentSlot HTML ps Aff msg -> Effect (RenderStateX RenderState)

newtype RenderState s msg ps o =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (VHTML msg ps) DOM.Node
    , renderChildRef :: Ref (ChildRenderer msg ps)
    }

mkSpec
  :: forall f msg ps
   . (InputF Unit (Coproduct f (Tuple msg) Unit) -> Effect Unit)
  -> Ref (ChildRenderer msg ps)
  -> DOM.Document
  -> V.VDomSpec
      (Array (VP.Prop (InputF Unit msg)))
      (ComponentSlot HTML ps Aff msg)
mkSpec handler renderChildRef document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (InputF Unit msg))) Unit
  buildAttributes = VP.buildProp (handler <<< map (right <<< flip Tuple unit))

  buildWidget
    :: V.VDomSpec
          (Array (VP.Prop (InputF Unit msg)))
          (ComponentSlot HTML ps Aff msg)
    -> V.Machine
          (ComponentSlot HTML ps Aff msg)
          DOM.Node
  buildWidget spec = EFn.mkEffectFn1 \slot -> do
    renderChild <- Ref.read renderChildRef
    rsx <- renderChild slot
    let node = getNode rsx
    pure (V.Step node patch done)

  patch
    :: V.Machine
         (ComponentSlot HTML ps Aff msg)
         DOM.Node
  patch = EFn.mkEffectFn1 \slot -> do
    renderChild <- Ref.read renderChildRef
    rsx <- renderChild slot
    let node = getNode rsx
    pure (V.Step node patch done)

  done :: Effect Unit
  done = pure unit

  getNode :: RenderStateX RenderState -> DOM.Node
  getNode = unRenderStateX (\(RenderState { node }) -> node)

runUI
  :: forall f i o
   . Component HTML f i o Aff
  -> i
  -> DOM.HTMLElement
  -> Aff (HalogenIO f o Aff)
runUI component i element = do
  document <- liftEffect $ HTMLDocument.toDocument <$> (DOM.document =<< DOM.window)
  AD.runUI (renderSpec document element) component i

renderSpec
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec HTML RenderState
renderSpec document container = { render, renderChild: identity, removeChild }
  where

  render
    :: forall s f msg ps o
     . (forall x. InputF x (Coproduct f (Tuple msg) x) -> Effect Unit)
    -> (ComponentSlot HTML ps Aff msg -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot HTML ps Aff msg) msg
    -> Maybe (RenderState s msg ps o)
    -> Effect (RenderState s msg ps o)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        renderChildRef <- Ref.new child
        let spec = mkSpec handler renderChildRef document
        machine <- EFn.runEffectFn1 (V.buildVDom spec) vdom
        let node = V.extract machine
        void $ DOM.appendChild node (HTMLElement.toNode container)
        pure $ RenderState { machine, node, renderChildRef }
      Just (RenderState { machine, node, renderChildRef }) -> do
        Ref.write child renderChildRef
        parent <- DOM.parentNode node
        nextSib <- DOM.nextSibling node
        machine' <- EFn.runEffectFn1 (V.step machine) vdom
        let newNode = V.extract machine'
        when (not unsafeRefEq node newNode) do
          substInParent newNode nextSib parent
        pure $ RenderState { machine: machine', node: newNode, renderChildRef }

removeChild :: forall s msg ps o. RenderState s msg ps o -> Effect Unit
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (\pn -> DOM.removeChild node pn) npn

substInParent :: DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> Effect Unit
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pure unit
