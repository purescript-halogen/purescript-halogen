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
  V.VDom (Array (Prop (Input action))) (ComponentSlot HTML slots Aff action)

type ChildRenderer action slots
  = ComponentSlotBox HTML slots Aff action -> Effect (RenderStateX RenderState)

newtype RenderState state action slots output =
  RenderState
    { node :: DOM.Node
    , machine :: V.Step (VHTML action slots) DOM.Node
    , renderChildRef :: Ref (ChildRenderer action slots)
    }

type HTMLThunk slots action =
  Thunk (HTML (ComponentSlot HTML slots Aff action)) action

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
          (ComponentSlot HTML slots Aff action)
          DOM.Node)
       (EFn.EffectFn2 (WidgetState slots action)
          (ComponentSlot HTML slots Aff action)
          (V.Step (ComponentSlot HTML slots Aff action) DOM.Node))
mkPatch = Fn.mkFn2 \renderChildRef buildWidget ->
  let
    patch
      :: EFn.EffectFn2 (WidgetState slots action)
        (ComponentSlot HTML slots Aff action)
        (V.Step (ComponentSlot HTML slots Aff action) DOM.Node)
    patch = EFn.mkEffectFn2 \st slot ->
      case st of
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
          (ComponentSlot HTML slots Aff action)
          DOM.Node
        )
        (ComponentSlotBox HTML slots Aff action)
        (V.Step (ComponentSlot HTML slots Aff action) DOM.Node)
renderComponentSlot = EFn.mkEffectFn3 \renderChildRef buildWidget cs -> do
  (renderChild :: ChildRenderer action slots) <- Ref.read renderChildRef
  (rsx :: RenderStateX RenderState) <- renderChild cs
  let node = getNode rsx
  pure $ V.mkStep $ V.Step node Nothing (Fn.runFn2 mkPatch renderChildRef buildWidget) widgetDone

widgetDone :: forall slots action . EFn.EffectFn1 (WidgetState slots action) Unit
widgetDone = EFn.mkEffectFn1 \st ->
  case st of
    Just step -> EFn.runEffectFn1 V.halt step
    _ -> pure unit

--------------

mkSpec
  :: forall action slots
   . (Input action -> Effect Unit)
  -> Ref (ChildRenderer action slots)
  -> DOM.Document
  -> V.VDomSpec
      (Array (VP.Prop (Input action)))
      (ComponentSlot HTML slots Aff action)
mkSpec handler renderChildRef document =
  V.VDomSpec { buildWidget, hydrateWidget, buildAttributes, hydrateAttributes, document }
  where

  buildAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input action))) Unit
  buildAttributes = VP.buildProp handler

  hydrateAttributes
    :: DOM.Element
    -> V.Machine (Array (VP.Prop (Input action))) Unit
  hydrateAttributes = VP.hydrateProp handler

  hydrateWidget
    :: V.VDomSpec
          (Array (VP.Prop (Input action)))
          (ComponentSlot HTML slots Aff action)
    -> DOM.Element
    -> V.Machine
          (ComponentSlot HTML slots Aff action)
          DOM.Node
  hydrateWidget spec elem = render
    where
    render :: V.Machine (ComponentSlot HTML slots Aff action) DOM.Node
    render = EFn.mkEffectFn1 \slot ->
      case slot of
        ComponentSlot cs ->
          EFn.runEffectFn3 renderComponentSlot renderChildRef render cs
        ThunkSlot t -> do
          step <- EFn.runEffectFn1 (Thunk.hydrateThunk unwrap spec elem) t
          pure $ V.mkStep $ V.Step (V.extract step) (Just step) (Fn.runFn2 mkPatch renderChildRef render) widgetDone

  buildWidget
    :: V.VDomSpec
          (Array (VP.Prop (Input action)))
          (ComponentSlot HTML slots Aff action)
    -> V.Machine
          (ComponentSlot HTML slots Aff action)
          DOM.Node
  buildWidget spec = render
    where
    render :: V.Machine (ComponentSlot HTML slots Aff action) DOM.Node
    render = EFn.mkEffectFn1 \slot ->
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
   . Component HTML query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
runUI component i container = do
  document <- findDocument
  AD.runUI (renderSpec document container) component i

hydrateUI
  :: forall query input output
   . Component HTML query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
hydrateUI component i rootElement = do
  document <- findDocument
  AD.runUI (renderSpecHydrate document rootElement) component i

renderSpecHydrate
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec HTML RenderState
renderSpecHydrate document element =
    { render
    , renderChild: identity
    , removeChild
    , dispose: removeChild
    }
  where

  render
    :: forall state action slots output
     . (Input action -> Effect Unit)
    -> (ComponentSlotBox HTML slots Aff action -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot HTML slots Aff action) action
    -> Maybe (RenderState state action slots output)
    -> Effect (RenderState state action slots output)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        renderChildRef <- Ref.new child
        let spec = mkSpec handler renderChildRef document
        machine <- EFn.runEffectFn1 (V.hydrateVDom spec (HTMLElement.toElement element)) vdom
        pure $ RenderState { machine, node: HTMLElement.toNode element, renderChildRef }
      Just renderState -> processNextRenderStateChange child (HTML vdom) renderState

renderSpec
  :: DOM.Document
  -> DOM.HTMLElement
  -> AD.RenderSpec HTML RenderState
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
    -> (ComponentSlotBox HTML slots Aff action -> Effect (RenderStateX RenderState))
    -> HTML (ComponentSlot HTML slots Aff action) action
    -> Maybe (RenderState state action slots output)
    -> Effect (RenderState state action slots output)
  render handler child (HTML vdom) =
    case _ of
      Nothing -> do
        renderChildRef <- Ref.new child
        let spec = mkSpec handler renderChildRef document
        machine <- EFn.runEffectFn1 (V.buildVDom spec) vdom
        let node = V.extract machine
        void $ DOM.appendChild node (HTMLElement.toNode container)
        pure $ RenderState { machine, node, renderChildRef }
      Just renderState -> processNextRenderStateChange child (HTML vdom) renderState

processNextRenderStateChange
  :: forall state action slots output
   . (ComponentSlotBox HTML slots Aff action -> Effect (RenderStateX RenderState))
  -> HTML (ComponentSlot HTML slots Aff action) action
  -> RenderState state action slots output
  -> Effect (RenderState state action slots output)
processNextRenderStateChange child (HTML vdom) (RenderState { machine, node, renderChildRef }) = do
  Ref.write child renderChildRef
  machine' <- EFn.runEffectFn2 V.step machine vdom
  let newNode = V.extract machine'
  when (not unsafeRefEq node newNode) do
    parent <- DOM.parentNode node
    nextSib <- DOM.nextSibling node
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
