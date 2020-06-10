module Halogen.Aff.Driver.HydrationImplementation where

import Prelude (Unit, bind, discard, flip, identity, pure, unit, void, when, ($), ($>), (<$>), (<<<), (=<<), (>>=))

import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List as L
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (warn)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlotBox, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Web.DOM.Element (Element) as DOM
import Halogen.Aff.Driver.RenderImplementation as RenderImplementation
import Halogen.Aff.Driver.RenderImplementation (RenderSpec)

runComponentHydrate
  :: forall f' i' o' h r
   . RenderSpec h r
  -> Boolean
  -> DOM.Element
  -> Ref LifecycleHandlers
  -> (o' -> Aff Unit)
  -> i'
  -> Component h f' i' o' Aff
  -> Effect (Ref (DriverStateX h r f' o'))
runComponentHydrate renderSpec isRoot currentElement lchs handler j = unComponent \c -> do
  lchs' <- RenderImplementation.newLifecycleHandlers
  var <- initDriverState c j handler lchs'
  pre <- Ref.read lchs
  Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
  unDriverStateX (renderHydrate renderSpec isRoot currentElement lchs <<< _.selfRef) =<< Ref.read var
  RenderImplementation.squashChildInitializers renderSpec isRoot lchs pre.initializers =<< Ref.read var -- TODO
  pure var

renderHydrate
  :: forall s f' act ps i' o' h r
   . RenderSpec h r
  -> Boolean
  -> DOM.Element
  -> Ref LifecycleHandlers
  -> Ref (DriverState h r s f' act ps i' o')
  -> Effect Unit
renderHydrate renderSpec isRoot currentElement lchs var = Ref.read var >>= \(DriverState ds) -> do
  traceM { message: "Halogen.Aff.Driver.HydrationImplementation.renderHydrate", var, currentElement }
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
    handler = Eval.queueOrRun pendingHandlers <<< void <<< Eval.evalF (RenderImplementation.render renderSpec false) selfRef -- TODO
    childHandler :: act -> Aff Unit
    childHandler = Eval.queueOrRun pendingQueries <<< handler <<< Input.Action
  rendering <-
    renderSpec.hydrate
      (RenderImplementation.handleAff <<< handler)
      (RenderImplementation.renderChild renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (renderChildHydrate renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (ds.component.render ds.state)
      currentElement
  children <- Ref.read ds.childrenOut
  childrenIn <- Ref.read ds.childrenIn
  Slot.foreachSlot childrenIn \(DriverStateRef childVar) -> do
    childDS <- Ref.read childVar
    renderStateX_ renderSpec.removeChild childDS
    RenderImplementation.finalize renderSpec isRoot lchs childDS
  flip Ref.modify_ ds.selfRef $ mapDriverState \ds' ->
    ds' { rendering = Just rendering, children = children }
  when shouldProcessHandlers do
    flip tailRecM unit \_ -> do
      handlers <- Ref.read pendingHandlers
      Ref.write (Just L.Nil) pendingHandlers
      traverse_ (RenderImplementation.handleAff <<< traverse_ fork <<< L.reverse) handlers
      mmore <- Ref.read pendingHandlers
      if maybe false L.null mmore
        then Ref.write Nothing pendingHandlers $> Done unit
        else pure $ Loop unit

renderChildHydrate
  :: forall ps act h r
   . RenderSpec h r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef h r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef h r))
  -> ComponentSlotBox h ps Aff act
  -> DOM.Element
  -> Effect (RenderStateX r)
renderChildHydrate renderSpec lchs handler childrenInRef childrenOutRef componentSlotBox element =
  unComponentSlot (\slot -> do
    traceM { message: "Halogen.Aff.Driver.HydrationImplementation.runUI renderChild 1", slot, childrenInRef }
    childrenIn <- slot.pop <$> Ref.read childrenInRef
    traceM { message: "Halogen.Aff.Driver.HydrationImplementation.runUI renderChild 2", childrenIn }
    var <- case childrenIn of
      Just (Tuple (DriverStateRef existing) childrenIn') -> throw "should not be called"
      Nothing -> do
        traceM { message: "Halogen.Aff.Driver.HydrationImplementation.runUI renderChild 3 -> childrenIn is nothing" }
        runComponentHydrate renderSpec false element lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
    isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
    when isDuplicate
      $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
    Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
    Ref.read var >>= renderStateX case _ of
      Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
      Just r -> pure (renderSpec.renderChild r)
  ) componentSlotBox
