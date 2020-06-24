module Halogen.VDom.Driver
  ( runUI
  , hydrateUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Aff.Util (findElementFirstChildOrThrow)
import Halogen.Component (Component, ComponentSlot(..), ComponentSlotBox)
import Halogen.HTML.Core (HTML(..), Prop)
import Halogen.Query.Input (Input)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Thunk as Thunk
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node, appendChild, removeChild, parentNode, nextSibling, insertBefore) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (HTMLElement) as DOM
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as DOM

type VHTML action slots =
  V.VDom (Array (Prop (Input action))) (ComponentSlot slots Aff action)

type ChildRenderer action slots
  = ComponentSlotBox slots Aff action -> Effect (RenderStateX RenderState)

type ChildRendererHydrate action slots
  = ComponentSlotBox slots Aff action -> DOM.Node -> Effect (RenderStateX RenderState)

newtype RenderState state action slots output =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (VHTML action slots) DOM.Node
    , renderChildRef :: Ref (ChildRenderer action slots)
    }

type HTMLThunk slots action =
  Thunk (HTML (ComponentSlot slots Aff action)) action

type WidgetState slots action =
  Maybe (V.Step (HTMLThunk slots action) DOM.Node)

-- | Utils

getNode :: RenderStateX RenderState -> DOM.Node
getNode = unRenderStateX (\(RenderState { node }) -> node)

mkPatch
  :: forall slots action
   . Fn.Fn2
       (Ref (ChildRenderer action slots))
       (V.Machine
          (ComponentSlot slots Aff action)
          DOM.Node)
       (EFn.EffectFn2
          (WidgetState slots action)
          (ComponentSlot slots Aff action)
          (V.Step (ComponentSlot slots Aff action) DOM.Node))
mkPatch = Fn.mkFn2 \renderChildRef buildWidget ->
  let
    patch
      :: EFn.EffectFn2
        (WidgetState slots action)
        (ComponentSlot slots Aff action)
        (V.Step (ComponentSlot slots Aff action) DOM.Node)
    patch = EFn.mkEffectFn2 \widgetState slot -> do
      case widgetState of
        Just step -> case slot of
          ComponentSlot cs -> do
            EFn.runEffectFn1 V.halt step
            EFn.runEffectFn3 renderComponentSlot renderChildRef buildWidget cs
          ThunkSlot t -> do
            step' <- EFn.runEffectFn2 V.step step t
            pure $ V.mkStep $ V.Step (V.extract step') (Just step') patch widgetDone
        _ -> EFn.runEffectFn1 buildWidget slot
  in patch

renderComponentSlot
  :: forall action slots
   . EFn.EffectFn3
        (Ref (ChildRenderer action slots))
        (V.Machine
          (ComponentSlot slots Aff action)
          DOM.Node
        )
        (ComponentSlotBox slots Aff action)
        (V.Step (ComponentSlot slots Aff action) DOM.Node)
renderComponentSlot = EFn.mkEffectFn3 \renderChildRef buildWidget componentSlotBox -> do
  (renderChild :: ChildRenderer action slots) <- Ref.read renderChildRef
  (rsx :: RenderStateX RenderState) <- renderChild componentSlotBox
  let node = getNode rsx
  pure $ V.mkStep $ V.Step node Nothing (Fn.runFn2 mkPatch renderChildRef buildWidget) widgetDone

renderComponentSlotHydrate
  :: forall action slots
   . EFn.EffectFn5
        DOM.Node
        (Ref (ChildRenderer action slots))
        (ChildRendererHydrate action slots)
        (V.Machine
          (ComponentSlot slots Aff action)
          DOM.Node
        )
        (ComponentSlotBox slots Aff action)
        (V.Step (ComponentSlot slots Aff action) DOM.Node)
renderComponentSlotHydrate = EFn.mkEffectFn5 \node renderChildRef renderChildHydrate buildWidget componentSlotBox -> do
  (rsx :: RenderStateX RenderState) <- renderChildHydrate componentSlotBox node -- use hydration only initially here, but on next steps (patch - ordinary render)
  pure $ V.mkStep $ V.Step node Nothing (Fn.runFn2 mkPatch renderChildRef buildWidget) widgetDone

widgetDone :: forall slots action . EFn.EffectFn1 (WidgetState slots action) Unit
widgetDone = EFn.mkEffectFn1 \st ->
  case st of
    Just step -> EFn.runEffectFn1 V.halt step
    _ -> pure unit

--------------

mkSpecWithHydration
  :: forall action slots
   . (Input action -> Effect Unit)
  -> Ref (ChildRenderer action slots)
  -> ChildRendererHydrate action slots
  -> DOM.Document
  -> V.VDomSpecWithHydration
      (Array (VP.Prop (Input action)))
      (ComponentSlot slots Aff action)
mkSpecWithHydration handler renderChildRef renderChildHydrate document =
  V.VDomSpecWithHydration { vdomSpec, hydrateWidget, hydrateAttributes }
  where

  vdomSpec = mkSpec handler renderChildRef document

  buildWidget = case vdomSpec of V.VDomSpec spec -> spec.buildWidget

  hydrateAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input action))) Unit
  hydrateAttributes = VP.hydrateProp handler

  hydrateWidget
    :: V.VDomSpecWithHydration
          (Array (VP.Prop (Input action)))
          (ComponentSlot slots Aff action)
    -> DOM.Node
    -> V.Machine
          (ComponentSlot slots Aff action)
          DOM.Node
  hydrateWidget specWithHydration node = render
    where
    render :: V.Machine (ComponentSlot slots Aff action) DOM.Node
    render = EFn.mkEffectFn1 \slot -> do
      case slot of
        ComponentSlot cs ->
          EFn.runEffectFn5 renderComponentSlotHydrate node renderChildRef renderChildHydrate (buildWidget vdomSpec) cs
        ThunkSlot t -> do
          step <- EFn.runEffectFn1 (Thunk.hydrateThunk unwrap specWithHydration node) t
          pure $ V.mkStep $ V.Step node (Just step) (Fn.runFn2 mkPatch renderChildRef (buildWidget vdomSpec)) widgetDone

mkSpec
  :: forall action slots
   . (Input action -> Effect Unit)
  -> Ref (ChildRenderer action slots)
  -> DOM.Document
  -> V.VDomSpec
      (Array (VP.Prop (Input action)))
      (ComponentSlot slots Aff action)
mkSpec handler renderChildRef document =
  V.VDomSpec { buildWidget, buildAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input action))) Unit
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec
          (Array (VP.Prop (Input action)))
          (ComponentSlot slots Aff action)
    -> V.Machine
          (ComponentSlot slots Aff action)
          DOM.Node
  buildWidget spec = render
    where
    render :: V.Machine (ComponentSlot slots Aff action) DOM.Node
    render = EFn.mkEffectFn1 \slot -> do
      case slot of
        ComponentSlot cs ->
          EFn.runEffectFn3 renderComponentSlot renderChildRef render cs
        ThunkSlot t -> do
          step <- EFn.runEffectFn1 (Thunk.buildThunk unwrap spec) t
          pure $ V.mkStep $ V.Step (V.extract step) (Just step) (Fn.runFn2 mkPatch renderChildRef render) widgetDone

findDocument :: Aff DOM.Document
findDocument = liftEffect $ HTMLDocument.toDocument <$> (DOM.document =<< DOM.window)

runUI
  :: forall query input output
   . Component query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
runUI component i container = do
  document <- findDocument
  AD.runUI (renderSpec document container) component i

hydrateUI
  :: forall query input output
   . Component query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
hydrateUI component i container = do
  document <- findDocument
  rootElement <- findElementFirstChildOrThrow container
  AD.hydrateUI (renderSpecWithHydration document container) component i (HTMLElement.toNode rootElement)

renderSpecWithHydration
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpecWithHydration RenderState
renderSpecWithHydration document container =
    { renderSpec: renderSpec document container
    , hydrate
    }
  where
  hydrate
    :: forall state action slots output
     . (Input action -> Effect Unit)
    -> (ChildRenderer action slots)
    -> (ChildRendererHydrate action slots)
    -> HTML (ComponentSlot slots Aff action) action
    -> DOM.Node
    -> Effect (RenderState state action slots output)
  hydrate handler renderChild renderChildHydrate (HTML vdom) node = do
    renderChildRef <- Ref.new renderChild
    let spec = mkSpecWithHydration handler renderChildRef renderChildHydrate document
    machine <- EFn.runEffectFn1 (V.hydrateVDom spec node) vdom
    pure $ RenderState { machine, node, renderChildRef }

renderSpec
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec RenderState
renderSpec document container =
    { render
    , renderChild: identity
    , removeChild
    , dispose: removeChild
    }
  where

  render
    :: forall state action slots output
     . (Input action -> Effect Unit)
    -> (ComponentSlotBox slots Aff action -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot slots Aff action) action
    -> Boolean
    -> Maybe (RenderState state action slots output)
    -> Effect (RenderState state action slots output)
  render handler renderChild (HTML vdom) isRoot =
    case _ of
      Nothing -> do
        renderChildRef <- Ref.new renderChild
        let spec = mkSpec handler renderChildRef document
        machine <- EFn.runEffectFn1 (V.buildVDom spec) vdom
        let node = V.extract machine
        when isRoot do
          void $ DOM.appendChild node (HTMLElement.toNode container)
        pure $ RenderState { machine, node, renderChildRef }
      Just renderState -> processNextRenderStateChange renderChild (HTML vdom) renderState

processNextRenderStateChange
  :: forall state action slots output
   . (ComponentSlotBox slots Aff action -> Effect (RenderStateX RenderState))
  -> HTML (ComponentSlot slots Aff action) action
  -> RenderState state action slots output
  -> Effect (RenderState state action slots output)
processNextRenderStateChange newRenderChild (HTML vdom) (RenderState { machine, node, renderChildRef }) = do
  parent <- DOM.parentNode node
  nextSib <- DOM.nextSibling node
  Ref.write newRenderChild renderChildRef
  machine' <- EFn.runEffectFn2 V.step machine vdom
  let newNode = V.extract machine'
  when (not unsafeRefEq node newNode) do
    substInParent newNode nextSib parent
  pure $ RenderState { machine: machine', node: newNode, renderChildRef }

removeChild :: forall state action slots output. RenderState state action slots output -> Effect Unit
removeChild (RenderState { node }) = do
  npn <- DOM.parentNode node
  traverse_ (\pn -> DOM.removeChild node pn) npn

substInParent :: DOM.Node -> Maybe DOM.Node -> Maybe DOM.Node -> Effect Unit
substInParent newNode (Just sib) (Just pn) = void $ DOM.insertBefore newNode sib pn
substInParent newNode Nothing (Just pn) = void $ DOM.appendChild newNode pn
substInParent _ _ _ = pure unit
