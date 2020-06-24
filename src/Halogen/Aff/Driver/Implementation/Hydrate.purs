module Halogen.Aff.Driver.Implementation.Hydrate where

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
import Debug.Trace (traceM, trace)
import Halogen.Aff.Driver.Implementation.Render as Render
import Halogen.Aff.Driver.Implementation.Types (RenderSpec)

runComponentHydrate
  :: forall f' i' o' r
   . RenderSpec r
  -> Boolean
  -> DOM.Node
  -> Ref LifecycleHandlers
  -> (o' -> Aff Unit)
  -> i'
  -> Component f' i' o' Aff
  -> Effect (Ref (DriverStateX r f' o'))
runComponentHydrate renderSpec isRoot currentNode lchs handler j = unComponent \c -> do
  lchs' <- Render.newLifecycleHandlers
  var <- initDriverState c j handler lchs'
  pre <- Ref.read lchs
  Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
  unDriverStateX (renderHydrate renderSpec isRoot currentNode lchs <<< _.selfRef) =<< Ref.read var
  Render.squashChildInitializers renderSpec isRoot lchs pre.initializers =<< Ref.read var -- TODO
  pure var

renderHydrate
  :: forall s f' act ps i' o' r
   . RenderSpec r
  -> Boolean
  -> DOM.Node
  -> Ref LifecycleHandlers
  -> Ref (DriverState r s f' act ps i' o')
  -> Effect Unit
renderHydrate renderSpec isRoot currentNode lchs var = Ref.read var >>= \(DriverState ds) -> do
  traceM { message: "Halogen.Aff.Driver.HydrationImplementation.renderHydrate", var, currentNode }
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
    handler = Eval.queueOrRun pendingHandlers <<< void <<< Eval.evalF (Render.render renderSpec false) selfRef -- TODO
    childHandler :: act -> Aff Unit
    childHandler = Eval.queueOrRun pendingQueries <<< handler <<< Input.Action
  rendering <-
    renderSpec.hydrate
      (Eval.handleAff <<< handler)
      (Render.renderChild renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (renderChildHydrate renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (ds.component.render ds.state)
      currentNode
  children <- Ref.read ds.childrenOut
  childrenIn <- Ref.read ds.childrenIn
  Slot.foreachSlot childrenIn \(DriverStateRef childVar) -> do
    childDS <- Ref.read childVar
    renderStateX_ renderSpec.removeChild childDS
    Render.finalize renderSpec isRoot lchs childDS
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

renderChildHydrate
  :: forall ps act r
   . RenderSpec r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> ComponentSlotBox ps Aff act
  -> DOM.Node
  -> Effect (RenderStateX r)
renderChildHydrate renderSpec lchs handler childrenInRef childrenOutRef componentSlotBox currentNode =
  unComponentSlot (\slot -> do
    traceM { message: "Halogen.Aff.Driver.HydrationImplementation.runUI renderChild 1", slot, childrenInRef }
    childrenIn <- slot.pop <$> Ref.read childrenInRef
    traceM { message: "Halogen.Aff.Driver.HydrationImplementation.runUI renderChild 2", childrenIn }
    var <- case childrenIn of
      Just (Tuple (DriverStateRef existing) childrenIn') -> throw "should not be called"
      Nothing -> do
        traceM { message: "Halogen.Aff.Driver.HydrationImplementation.runUI renderChild 3 -> childrenIn is nothing" }
        runComponentHydrate renderSpec false currentNode lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
    isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
    when isDuplicate
      $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
    Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
    Ref.read var >>= renderStateX case _ of
      Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
      Just r -> pure (renderSpec.renderChild r)
  ) componentSlotBox
