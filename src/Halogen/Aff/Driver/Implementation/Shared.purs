module Halogen.Aff.Driver.Implementation.Shared where

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
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, ComponentSlotSpec, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM

runComponent
  :: forall f i o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> (o -> Aff Unit)
  -> i
  -> (DriverStateX r f o -> Effect Unit)
  -> Component f i o Aff
  -> Effect (Ref (DriverStateX r f o))
runComponent renderSpec isRoot lchs handler j runRender = unComponent \c -> do
  lchs' <- newLifecycleHandlers
  var <- initDriverState c j handler lchs'
  pre <- Ref.read lchs
  Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
  runRender =<< Ref.read var
  squashChildInitializers renderSpec isRoot lchs pre.initializers =<< Ref.read var
  pure var

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

renderFromRender
  :: forall s f act ps i o r
   . RenderSpec r
  -> Boolean
  -> Ref LifecycleHandlers
  -> Ref (DriverState r s f act ps i o)
  -> Effect Unit
renderFromRender = unsafeCoerce unit -- TODO

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
    let parentInitializer = Eval.evalM (renderFromRender renderSpec isRoot) st.selfRef (st.component.eval (HQ.Initialize unit))
    Ref.modify_ (\handlers ->
      { initializers: (do
          parSequence_ (L.reverse handlers.initializers)
          parentInitializer
          liftEffect do
            handlePending st.pendingQueries
            handlePending st.pendingOuts) : preInits
      , finalizers: handlers.finalizers
      }) lchs

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (Eval.handleAff <<< traverse_ fork <<< L.reverse)

renderChild
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
renderChild renderSpec lchs handler childrenInRef childrenOutRef renderWithExistenShildrenState renderNew =
  unComponentSlot \slot -> do
    childrenIn <- slot.pop <$> Ref.read childrenInRef
    var <- case childrenIn of
      Just childrenIn' -> renderWithExistenShildrenState slot childrenIn'
      Nothing -> renderNew slot
    isDuplicate <- isJust <<< slot.get <$> Ref.read childrenOutRef
    when isDuplicate
      $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
    Ref.modify_ (slot.set $ DriverStateRef var) childrenOutRef
    Ref.read var >>= renderStateX case _ of
      Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
      Just r -> pure (renderSpec.renderChild r)
