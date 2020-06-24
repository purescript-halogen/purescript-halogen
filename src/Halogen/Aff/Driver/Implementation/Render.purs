module Halogen.Aff.Driver.Implementation.Render where

import Prelude

import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, sequence_, traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM, trace)
import Effect (Effect)
import Effect.Aff (Aff, killFiber)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event as Event
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.Implementation.Types (RenderSpec)
import Halogen.Aff.Driver.Implementation.Utils as Utils
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, DriverStateRec, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, ComponentSlotSpec, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM

-- | Functions, implemented using the `XxxxImplementation` functions, but specific to rendering, and not hydration.

render
  :: forall s f act ps i o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> Ref (DriverState r s f act ps i o)
  -> Effect Unit
render renderSpec isRoot lchs var = renderImplementation renderSpec isRoot lchs var runRender
  where
  runRender :: (Input act -> Aff Unit) -> (act -> Aff Unit) -> DriverStateRec r s f act ps i o -> Effect (r s act ps o)
  runRender handler childHandler ds =
    renderSpec.render
      (Eval.handleAff <<< handler)
      (renderChild renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (ds.component.render ds.state)
      isRoot
      ds.rendering

renderChild
  :: forall ps act r
   . RenderSpec r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> ComponentSlotBox ps Aff act
  -> Effect (RenderStateX r)
renderChild renderSpec lchs handler childrenInRef childrenOutRef = renderChildImplementation renderSpec lchs handler childrenInRef childrenOutRef renderWithExistingChildrenState renderNew
  where
  renderWithExistingChildrenState :: forall query input output. ComponentSlotSpec query input output ps Aff act -> Tuple (DriverStateRef r query output) (Slot.SlotStorage ps (DriverStateRef r)) -> Effect (Ref (DriverStateX r query output))
  renderWithExistingChildrenState slot (Tuple (DriverStateRef existing) childrenIn') = do
    Ref.write childrenIn' childrenInRef
    dsx <- Ref.read existing
    unDriverStateX (\st -> do
      flip Ref.write st.handlerRef $ maybe (pure unit) handler <<< slot.output
      Eval.handleAff $ Eval.evalM (render renderSpec false) st.selfRef (st.component.eval (HQ.Receive slot.input unit))) dsx
    pure existing

  renderNew :: forall query input output. ComponentSlotSpec query input output ps Aff act -> Effect (Ref (DriverStateX r query output))
  renderNew slot = runComponent renderSpec false lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component

runComponent
  :: forall f i o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> (o -> Aff Unit)
  -> i
  -> Component f i o Aff
  -> Effect (Ref (DriverStateX r f o))
runComponent renderSpec isRoot lchs handler j = runComponentImplementation renderSpec isRoot lchs handler j runRender
  where
    runRender :: DriverStateX r f o -> Effect Unit
    runRender = unDriverStateX (render renderSpec isRoot lchs <<< _.selfRef)

-- | Functions, containing common code used for implementing hydration and rendering functions

renderImplementation
  :: forall s f act ps i o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> Ref (DriverState r s f act ps i o)
  -> ((Input act -> Aff Unit) -> (act -> Aff Unit) -> DriverStateRec r s f act ps i o -> Effect (r s act ps o))
  -> Effect Unit
renderImplementation renderSpec isRoot lchs var runRender = Ref.read var >>= \(DriverState ds) -> do
  shouldProcessHandlers <- isNothing <$> Ref.read ds.pendingHandlers
  when shouldProcessHandlers $ Ref.write (Just L.Nil) ds.pendingHandlers
  Ref.write Slot.empty ds.childrenOut
  Ref.write ds.children ds.childrenIn
  let
    -- The following 3 defs are working around a capture bug, see #586
    pendingHandlers :: Ref (Maybe (List (Aff Unit)))
    pendingHandlers = identity ds.pendingHandlers

    pendingQueries :: Ref (Maybe (List (Aff Unit)))
    pendingQueries = identity ds.pendingQueries

    selfRef :: Ref (DriverState r s f act ps i o)
    selfRef = identity ds.selfRef

    handler :: Input act -> Aff Unit
    handler = Eval.queueOrRun pendingHandlers <<< void <<< Eval.evalF (render renderSpec false) selfRef

    childHandler :: act -> Aff Unit
    childHandler = Eval.queueOrRun pendingQueries <<< handler <<< Input.Action
  (rendering :: r s act ps o) <- runRender handler childHandler ds
  children <- Ref.read ds.childrenOut
  childrenIn <- Ref.read ds.childrenIn
  Slot.foreachSlot childrenIn \(DriverStateRef childVar) -> do
    childDS <- Ref.read childVar
    renderStateX_ renderSpec.removeChild childDS
    finalize renderSpec isRoot lchs childDS
  flip Ref.modify_ ds.selfRef $ mapDriverState \ds' ->
    ds' { rendering = Just rendering, children = children }
  when shouldProcessHandlers do
    flip tailRecM unit \_ -> do
      handlers <- Ref.read pendingHandlers
      Ref.write (Just L.Nil) pendingHandlers
      traverse_ (Eval.handleAff <<< traverse_ fork <<< L.reverse) handlers
      mmore <- Ref.read pendingHandlers
      if maybe false L.null mmore
        then Ref.write Nothing pendingHandlers $> Done unit
        else pure $ Loop unit

renderChildImplementation
  :: forall ps act r
   . RenderSpec r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> (forall query input output. ComponentSlotSpec query input output ps Aff act -> Tuple (DriverStateRef r query output) (Slot.SlotStorage ps (DriverStateRef r)) -> Effect (Ref (DriverStateX r query output)))
  -> (forall query input output. ComponentSlotSpec query input output ps Aff act -> Effect (Ref (DriverStateX r query output)))
  -> ComponentSlotBox ps Aff act
  -> Effect (RenderStateX r)
renderChildImplementation renderSpec lchs handler childrenInRef childrenOutRef renderWithExistingChildrenState renderNew =
  unComponentSlot \slot -> do
    childrenIn <- slot.pop <$> Ref.read childrenInRef
    var <- case childrenIn of
      Just childrenIn' -> renderWithExistingChildrenState slot childrenIn'
      Nothing -> renderNew slot
    isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
    when isDuplicate
      $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
    Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
    Ref.read var >>= renderStateX case _ of
      Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
      Just r -> pure (renderSpec.renderChild r)

runComponentImplementation
  :: forall f i o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> (o -> Aff Unit)
  -> i
  -> (DriverStateX r f o -> Effect Unit)
  -> Component f i o Aff
  -> Effect (Ref (DriverStateX r f o))
runComponentImplementation renderSpec isRoot lchs handler j runRender = unComponent \c -> do
  lchs' <- Utils.newLifecycleHandlers
  var <- initDriverState c j handler lchs'
  pre <- Ref.read lchs
  Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
  runRender =<< Ref.read var
  squashChildInitializers renderSpec isRoot lchs pre.initializers =<< Ref.read var
  pure var

-- | Utility function that use rendering inside

squashChildInitializers
  :: forall f o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> L.List (Aff Unit)
  -> DriverStateX r f o
  -> Effect Unit
squashChildInitializers renderSpec isRoot lchs preInits =
  unDriverStateX \st -> do
    let parentInitializer = Eval.evalM (render renderSpec isRoot) st.selfRef (st.component.eval (HQ.Initialize unit))
    Ref.modify_ (\handlers ->
      { initializers: (do
          parSequence_ (L.reverse handlers.initializers)
          parentInitializer
          liftEffect do
            Utils.handlePending st.pendingQueries
            Utils.handlePending st.pendingOuts) : preInits
      , finalizers: handlers.finalizers
      }) lchs

finalize
  :: forall f o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> DriverStateX r f o
  -> Effect Unit
finalize renderSpec isRoot lchs = do
  unDriverStateX \st -> do
    Utils.cleanupSubscriptionsAndForks (DriverState st)
    let f = Eval.evalM (render renderSpec isRoot) st.selfRef (st.component.eval (HQ.Finalize unit))
    Ref.modify_ (\handlers ->
      { initializers: handlers.initializers
      , finalizers: f : handlers.finalizers
      }) lchs
    Slot.foreachSlot st.children \(DriverStateRef ref) -> do
      dsx <- Ref.read ref
      finalize renderSpec isRoot lchs dsx

