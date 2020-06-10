module Halogen.Aff.Driver.RenderImplementation where

import Prelude (Unit, bind, const, discard, flip, identity, map, pure, unit, void, when, ($), ($>), (+), (<$>), (<<<), (=<<), (>>=))

import Control.Coroutine as CR
import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.Either (Either(..), either)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, killFiber, launchAff_, runAff_, try)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw, throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Web.DOM.Element (Element) as DOM

-- | `RenderSpec` allows for alternative driver implementations without the need
-- | to provide all of the driver machinery again, focusing just on the code
-- | needed to render components.
-- |
-- | The type variables are as follows:
-- | - `h` is the type of value being rendered (`Halogen.HTML.Core.HTML`, for
-- |   example).
-- | - `r` is the type for the "render state" for the driver. This is a value
-- |   that is stored for each component, that allows the driver to persist
-- |   state between each rendering of a component. This will differ entirely
-- |   for each driver. `r` accepts a number of parameters that will be
-- |   explained below.
-- |
-- | The "inner" type variables, used by `r` and the other functions are as
-- | follows:
-- | - `s` is the state type for the component.
-- | - `act` is the action type for the component
-- | - `ps` is the set of slots for addressing child components
-- | - `o` is the output message type for the component
-- |
-- | Note that none of these variables can escape `RenderSpec`'s functions. They
-- | need to be instantiated with each function call, as the same `RenderSpec`
-- | is used to deal with all components in the hierarchy.
-- |
-- | The `render` function is the main part of the spec, it accepts:
-- | - A "handler" function, for evaluating component queries. This is used to
-- |   implement event handlers in HTML-based drivers.
-- | - A "child" function for dealing with the rendering of children, returning
-- |   the render state for the child component in an existentially hidden
-- |   package. This return value would commonly be used to extract the rendered
-- |   subtree for the child to graft it in place of the child slot. The
-- |   existential package can be unwrapped with `Halogen.Aff.Driver.State.unRenderStateX`.
-- | - The `h` value to render, parameterised by the slot type for the
-- |   component's children. This slot type is what the "child" function
-- |   accepts.
-- | - The previous render state for the component. If the component has not
-- |   previously been initalized, this will be `Nothing`.
-- |
-- | The render function then returns the updated (or initial) render state for
-- | the component, which will be fed back into `render` the next time the
-- | component needs to update.
-- |
-- | The `renderChild` function's behaviour will be highly dependant on the
-- | particular driver implementing `RenderSpec`. Its purpose is to take a
-- | driver render state for a component and produce a new one that may remap
-- | the rendered value to be something more suitable for grafting during
-- | `render` of the parent. For the built-in `halogen-vdom` driver this is
-- | just `identity`. For the `virtual-dom` driver it wraps the rendered HTML
-- | in a widget, to prevent the `virtual-dom` algorithm from re-diffing
-- | values that we know are unchanged.
-- |
-- | The `removeChild` function is for drivers that need to perform some special
-- | cleanup when removing a component from the hierarchy. In the `halogen-vdom`
-- | driver this actually performs the `removeChild` from the DOM. For the
-- | `virtual-dom` driver nothing needs to happen here, so it is
-- | `const (pure unit)`.
-- |
-- | The `dispose` function is called when the top level component is disposed of
-- | via `HalogenIO`.
type RenderSpec h r =
  { render
      :: forall s act ps o
       . (Input act -> Effect Unit)
      -> (ComponentSlotBox h ps Aff act -> Effect (RenderStateX r))
      -> h (ComponentSlot h ps Aff act) act
      -> Boolean
      -> Maybe (r s act ps o)
      -> Effect (r s act ps o)
  , hydrate
      :: forall s act ps o
       . (Input act -> Effect Unit)
      -> (ComponentSlotBox h ps Aff act -> Effect (RenderStateX r))
      -> (ComponentSlotBox h ps Aff act -> DOM.Element -> Effect (RenderStateX r))
      -> h (ComponentSlot h ps Aff act) act
      -> DOM.Element
      -> Effect (r s act ps o)
  , renderChild :: forall s act ps o. r s act ps o -> r s act ps o
  , removeChild :: forall s act ps o. r s act ps o -> Effect Unit
  , dispose :: forall s act ps o. r s act ps o -> Effect Unit
  }

evalDriver
  :: forall s f' act ps i' o' h r
   . RenderSpec h r
  -> Ref Boolean
  -> Ref (DriverState h r s f' act ps i' o')
  -> forall a. (f' a -> Aff (Maybe a))
evalDriver renderSpec disposed ref q =
  liftEffect (Ref.read disposed) >>=
    if _
      then pure Nothing
      else Eval.evalQ (render renderSpec true) ref q -- true because `evalDriver` is used only on root container

rootHandler :: forall o. Ref (M.Map Int (AV.AVar o)) -> o -> Aff Unit
rootHandler ref message = do
  listeners <- liftEffect $ Ref.read ref
  traverse_ fork $ map (AV.put message) listeners

subscribe
  :: forall o
   . Ref Int
  -> Ref (M.Map Int (AV.AVar o))
  -> CR.Consumer o Aff Unit
  -> Aff Unit
subscribe fresh ref consumer = do
  inputVar <- AV.empty
  listenerId <- liftEffect do
    listenerId <- Ref.read fresh
    Ref.modify_ (_ + 1) fresh
    Ref.modify_ (M.insert listenerId inputVar) ref
    pure listenerId
  let producer = CR.producer $ either (const (Right unit)) Left <$> try (AV.take inputVar)
  void $ fork do
    CR.runProcess (CR.connect producer consumer)
    liftEffect $ Ref.modify_ (M.delete listenerId) ref
    AV.kill (error "ended") inputVar

runComponent
  :: forall f' i' o' h r
   . RenderSpec h r
  -> Boolean
  -> Ref LifecycleHandlers
  -> (o' -> Aff Unit)
  -> i'
  -> Component h f' i' o' Aff
  -> Effect (Ref (DriverStateX h r f' o'))
runComponent renderSpec isRoot lchs handler j = unComponent \c -> do
  lchs' <- newLifecycleHandlers
  var <- initDriverState c j handler lchs'
  pre <- Ref.read lchs
  Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
  unDriverStateX (render renderSpec isRoot lchs <<< _.selfRef) =<< Ref.read var
  squashChildInitializers renderSpec isRoot lchs pre.initializers =<< Ref.read var
  pure var

render
  :: forall s f' act ps i' o' h r
   . RenderSpec h r
  -> Boolean
  -> Ref LifecycleHandlers
  -> Ref (DriverState h r s f' act ps i' o')
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
      (handleAff <<< handler)
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
      traverse_ (handleAff <<< traverse_ fork <<< L.reverse) handlers
      mmore <- Ref.read pendingHandlers
      if maybe false L.null mmore
        then Ref.write Nothing pendingHandlers $> Done unit
        else pure $ Loop unit

renderChild
  :: forall ps act h r
   . RenderSpec h r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef h r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef h r))
  -> ComponentSlotBox h ps Aff act
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
          handleAff $ Eval.evalM (render renderSpec false) st.selfRef (st.component.eval (HQ.Receive slot.input unit))) dsx
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
  :: forall f' o' h r
   . RenderSpec h r
  -> Boolean
  -> Ref LifecycleHandlers
  -> L.List (Aff Unit)
  -> DriverStateX h r f' o'
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
  :: forall f' o' h r
   . RenderSpec h r
  -> Boolean
  -> Ref LifecycleHandlers
  -> DriverStateX h r f' o'
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

dispose :: forall f' o' h r
   . RenderSpec h r
  -> Ref Boolean
  -> Ref LifecycleHandlers
  -> DriverStateX h r f' o'
  -> Ref (M.Map Int (AV.AVar o'))
  -> Aff Unit
dispose renderSpec disposed lchs dsx subsRef = Eval.handleLifecycle lchs do
  Ref.read disposed >>=
    if _
      then pure unit
      else do
        Ref.write true disposed
        traverse_ (launchAff_ <<< AV.kill (error "disposed")) =<< Ref.read subsRef
        finalize renderSpec true lchs dsx
        unDriverStateX (traverse_ renderSpec.dispose <<< _.rendering) dsx

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (handleAff <<< traverse_ fork <<< L.reverse)

cleanupSubscriptionsAndForks
  :: forall h r s f act ps i o
   . DriverState h r s f act ps i o
  -> Effect Unit
cleanupSubscriptionsAndForks (DriverState ds) = do
  traverse_ (handleAff <<< traverse_ (fork <<< ES.finalize)) =<< Ref.read ds.subscriptions
  Ref.write Nothing ds.subscriptions
  traverse_ (handleAff <<< killFiber (error "finalized")) =<< Ref.read ds.forks
  Ref.write M.empty ds.forks

-- We could perhaps do something more intelligent now this isn't baked into
-- the virtual-dom rendering. It hasn't really been a problem so far though.
handleAff :: forall a. Aff a -> Effect Unit
handleAff = runAff_ (either throwException (const (pure unit)))
