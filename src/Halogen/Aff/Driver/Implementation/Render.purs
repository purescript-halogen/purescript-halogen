module Halogen.Aff.Driver.Implementation.Render where

import Prelude

import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, sequence_, traverse_)
import Data.Tuple (Tuple(..))
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
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM
import Halogen.Aff.Driver.Implementation.Types (RenderSpec)
import Debug.Trace (traceM, trace)

evalDriver
  :: forall s f' act ps i' o' r
   . RenderSpec r
  -> Ref Boolean
  -> Ref (DriverState r s f' act ps i' o')
  -> forall a. (f' a -> Aff (Maybe a))
evalDriver renderSpec disposed ref q =
  liftEffect (Ref.read disposed) >>=
    if _
      then pure Nothing
      else Eval.evalQ (render renderSpec true) ref q -- true because `evalDriver` is used only on root container

runComponent
  :: forall f' i' o' r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> (o' -> Aff Unit)
  -> i'
  -> Component f' i' o' Aff
  -> Effect (Ref (DriverStateX r f' o'))
runComponent renderSpec isRoot lchs handler j = unComponent \c -> do
  lchs' <- newLifecycleHandlers
  var <- initDriverState c j handler lchs'
  pre <- Ref.read lchs
  Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
  unDriverStateX (render renderSpec isRoot lchs <<< _.selfRef) =<< Ref.read var
  squashChildInitializers renderSpec isRoot lchs pre.initializers =<< Ref.read var
  pure var

render
  :: forall s f' act ps i' o' r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> Ref (DriverState r s f' act ps i' o')
  -> Effect Unit
render renderSpec isRoot lchs var = Ref.read var >>= \(DriverState ds) -> do
  traceM { message: "Halogen.Aff.Driver.runUI renderChild is called!!!!!!!!!!!!!!!!!!!!!", isRoot, ds }
  shouldProcessHandlers <- isNothing <$> Ref.read ds.pendingHandlers
  when shouldProcessHandlers $ Ref.write (Just L.Nil) ds.pendingHandlers
  Ref.write Slot.empty ds.childrenOut
  Ref.write ds.children ds.childrenIn
  let
    -- The following 3 defs are working around a capture bug, see #586
    pendingHandlers = identity ds.pendingHandlers
    pendingQueries = identity ds.pendingQueries
    selfRef = identity ds.selfRef
    handler :: Input act -> Aff Unit
    handler = Eval.queueOrRun pendingHandlers <<< void <<< Eval.evalF (render renderSpec isRoot) selfRef
    childHandler :: act -> Aff Unit
    childHandler = Eval.queueOrRun pendingQueries <<< handler <<< Input.Action
  rendering <-
    renderSpec.render
      (Eval.handleAff <<< handler)
      (renderChild renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (ds.component.render ds.state)
      isRoot
      ds.rendering
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

renderChild
  :: forall ps act r
   . RenderSpec r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> ComponentSlotBox ps Aff act
  -> Effect (RenderStateX r)
renderChild renderSpec lchs handler childrenInRef childrenOutRef =
  unComponentSlot \slot -> do
    traceM { message: "Halogen.Aff.Driver.runUI renderChild 1", slot, childrenInRef }
    childrenIn <- slot.pop <$> Ref.read childrenInRef
    traceM { message: "Halogen.Aff.Driver.runUI renderChild 2", childrenIn }
    var <- case childrenIn of
      Just (Tuple (DriverStateRef existing) childrenIn') -> do
        traceM { message: "Halogen.Aff.Driver.runUI renderChild 3 -> childrenIn is just" }
        Ref.write childrenIn' childrenInRef
        dsx <- Ref.read existing
        unDriverStateX (\st -> do
          flip Ref.write st.handlerRef $ maybe (pure unit) handler <<< slot.output
          Eval.handleAff $ Eval.evalM (render renderSpec false) st.selfRef (st.component.eval (HQ.Receive slot.input unit))) dsx
        pure existing
      Nothing -> do
        traceM { message: "Halogen.Aff.Driver.runUI renderChild 3 -> childrenIn is nothing" }
        runComponent renderSpec false lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
    isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
    when isDuplicate
      $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
    Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
    Ref.read var >>= renderStateX case _ of
      Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
      Just r -> pure (renderSpec.renderChild r)

squashChildInitializers
  :: forall f' o' r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> L.List (Aff Unit)
  -> DriverStateX r f' o'
  -> Effect Unit
squashChildInitializers renderSpec isRoot lchs preInits =
  unDriverStateX \st -> do
    let parentInitializer = Eval.evalM (render renderSpec isRoot) st.selfRef (st.component.eval (HQ.Initialize unit))
    Ref.modify_ (\handlers ->
      { initializers: (do
          parSequence_ (L.reverse handlers.initializers)
          parentInitializer
          liftEffect do
            handlePending st.pendingQueries
            handlePending st.pendingOuts) : preInits
      , finalizers: handlers.finalizers
      }) lchs

finalize
  :: forall f' o' r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> DriverStateX r f' o'
  -> Effect Unit
finalize renderSpec isRoot lchs = do
  unDriverStateX \st -> do
    cleanupSubscriptionsAndForks (DriverState st)
    let f = Eval.evalM (render renderSpec isRoot) st.selfRef (st.component.eval (HQ.Finalize unit))
    Ref.modify_ (\handlers ->
      { initializers: handlers.initializers
      , finalizers: f : handlers.finalizers
      }) lchs
    Slot.foreachSlot st.children \(DriverStateRef ref) -> do
      dsx <- Ref.read ref
      finalize renderSpec isRoot lchs dsx

dispose :: forall f' o' r
   . RenderSpec r
  -> Ref Boolean
  -> Ref LifecycleHandlers
  -> DriverStateX r f' o'
  -> Aff Unit
dispose renderSpec disposed lchs dsx = Eval.handleLifecycle lchs do
  Ref.read disposed >>=
    if _
      then pure unit
      else do
        Ref.write true disposed
        finalize renderSpec true lchs dsx
        unDriverStateX (traverse_ renderSpec.dispose <<< _.rendering) dsx

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (Eval.handleAff <<< traverse_ fork <<< L.reverse)

cleanupSubscriptionsAndForks
  :: forall r s f act ps i o
   . DriverState r s f act ps i o
  -> Effect Unit
cleanupSubscriptionsAndForks (DriverState ds) = do
  traverse_ sequence_ =<< Ref.read ds.subscriptions
  Ref.write Nothing ds.subscriptions
  traverse_ (Eval.handleAff <<< killFiber (error "finalized")) =<< Ref.read ds.forks
  Ref.write M.empty ds.forks
