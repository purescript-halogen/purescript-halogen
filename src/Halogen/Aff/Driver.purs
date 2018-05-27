module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.Either (Either(..), either)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw, throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval (eval, handleLifecycle, queuingHandler)
import Halogen.Aff.Driver.State (LifecycleHandlers, DriverState(..), DriverStateX, RenderStateX, initDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox)
import Halogen.Query.InputF (InputF(..))

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
-- | - `eff` is the type variable for the openect row.
-- |
-- | The "inner" type variables, used by `r` and the other functions are as
-- | follows:
-- | - `s` is the state type for the component.
-- | - `f` is the query algebra for the component.
-- | - `g` is the query algebra for the component's children.
-- | - `p` is the slot address value type.
-- | - `o` is the output message type for the component.
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
-- | just `id`. For the `virtual-dom` driver it wraps the rendered HTML
-- | in a widget, to prevent the `virtual-dom` algorithm from re-diffing
-- | values that we know are unchanged.
-- |
-- | The `removeChild` function is for drivers that need to perform some special
-- | cleanup when removing a component from the hierarchy. In the `halogen-vdom`
-- | driver this actually performs the `removeChild` from the DOM. For the
-- | `virtual-dom` driver nothing needs to happen here, so it is
-- | `const (pure unit)`.
type RenderSpec h r =
  { render
      :: forall s f g p o
       . (forall x. InputF x (f x) -> Effect Unit)
      -> (ComponentSlot h g Aff p (f Unit) -> Effect (RenderStateX r))
      -> h (ComponentSlot h g Aff p (f Unit)) (f Unit)
      -> Maybe (r s f g p o)
      -> Effect (r s f g p o)
  , renderChild :: forall s f g p o. r s f g p o -> r s f g p o
  , removeChild :: forall s f g p o. r s f g p o -> Effect Unit
  }

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

runUI
  :: forall h r f i o
   . RenderSpec h r
  -> Component h f i o Aff
  -> i
  -> Aff (HalogenIO f o Aff)
runUI renderSpec component i = do
  lchs <- liftEffect newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  handleLifecycle lchs do
    listeners <- Ref.new M.empty
    runComponent lchs (rootHandler listeners) i Just component
      >>= Ref.read
      >>= unDriverStateX \st ->
        pure
          { query: evalDriver st.selfRef st.prjQuery
          , subscribe: subscribe fresh listeners
          }

  where

  evalDriver
    :: forall s f' z' g p i' o'
     . Ref (DriverState h r s f' z' g p i' o')
    -> (forall x. f' x -> Maybe (z' x))
    -> f'
    ~> Aff
  evalDriver ref prjQuery q =
    case prjQuery q of
      Just q' -> evalF ref (Query q')
      Nothing -> liftEffect $ throwException (error "Halogen internal error: query projection failed in runUI'")

  evalF
    :: forall s f' z' g p i' o' a
     . Ref (DriverState h r s f' z' g p i' o')
    -> InputF a (z' a)
    -> Aff a
  evalF ref = eval render ref

  rootHandler
    :: Ref (M.Map Int (AV.AVar o))
    -> o
    -> Aff Unit
  rootHandler ref message = do
    listeners <- liftEffect $ Ref.read ref
    traverse_ fork $ map (AV.put message) listeners

  subscribe
    :: Ref Int
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
    let producer = CR.producer (Left <$> AV.take inputVar)
    void $ fork do
      CR.runProcess (CR.connect producer consumer)
      liftEffect $ Ref.modify_ (M.delete listenerId) ref
      AV.kill (error "ended") inputVar

  runComponent
    :: forall z f' i' o'
     . Ref LifecycleHandlers
    -> (o' -> Aff Unit)
    -> i'
    -> (forall x. f' x -> Maybe (z x))
    -> Component h z i' o' Aff
    -> Effect (Ref (DriverStateX h r f'))
  runComponent lchs handler j prjQuery = unComponent \c -> do
    lchs' <- newLifecycleHandlers
    var <- initDriverState c j handler prjQuery lchs'
    pre <- Ref.read lchs
    Ref.write { initializers: L.Nil, finalizers: pre.finalizers } lchs
    unDriverStateX (render lchs <<< _.selfRef) =<< Ref.read var
    squashChildInitializers lchs pre.initializers =<< Ref.read var
    pure var

  render
    :: forall s f' z' g p i' o'
     . Ref LifecycleHandlers
    -> Ref (DriverState h r s f' z' g p i' o')
    -> Effect Unit
  render lchs var = Ref.read var >>= \(DriverState ds) -> do
    shouldProcessHandlers <- isNothing <$> Ref.read ds.pendingHandlers
    when shouldProcessHandlers $ Ref.write (Just L.Nil) ds.pendingHandlers
    Ref.write M.empty ds.childrenOut
    Ref.write ds.children ds.childrenIn
    let
      handler :: forall x. InputF x (z' x) -> Aff Unit
      handler = queuingHandler (void <<< evalF ds.selfRef) ds.pendingHandlers
      childHandler :: forall x. z' x -> Aff Unit
      childHandler = queuingHandler (handler <<< Query) ds.pendingQueries
    rendering <-
      renderSpec.render
        (handleAff <<< handler)
        (renderChild lchs childHandler ds.component.mkOrdBox ds.childrenIn ds.childrenOut)
        (ds.component.render ds.state)
        ds.rendering
    children <- Ref.read ds.childrenOut
    Ref.read ds.childrenIn >>= traverse_ \childVar -> do
      childDS <- Ref.read childVar
      renderStateX_ renderSpec.removeChild childDS
      finalize lchs childDS
    Ref.modify_ (\(DriverState ds') ->
      DriverState
        { rendering: Just rendering
        , children
        , component: ds'.component
        , state: ds'.state
        , refs: ds'.refs
        , childrenIn: ds'.childrenIn
        , childrenOut: ds'.childrenOut
        , selfRef: ds'.selfRef
        , handler: ds'.handler
        , pendingQueries: ds'.pendingQueries
        , pendingOuts: ds'.pendingOuts
        , pendingHandlers: ds'.pendingHandlers
        , prjQuery: ds'.prjQuery
        , fresh: ds'.fresh
        , subscriptions: ds'.subscriptions
        , lifecycleHandlers: ds'.lifecycleHandlers
        }) ds.selfRef
    when shouldProcessHandlers do
      flip tailRecM unit \_ -> do
        handlers <- Ref.read ds.pendingHandlers
        Ref.write (Just L.Nil) ds.pendingHandlers
        traverse_ (handleAff <<< traverse_ fork <<< L.reverse) handlers
        mmore <- Ref.read ds.pendingHandlers
        if maybe false L.null mmore
          then Ref.write Nothing ds.pendingHandlers $> Done unit
          else pure $ Loop unit

  renderChild
    :: forall f' g p
     . Ref LifecycleHandlers
    -> (forall x. f' x -> Aff Unit)
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g)))
    -> ComponentSlot h g Aff p (f' Unit)
    -> Effect (RenderStateX r)
  renderChild lchs handler mkOrdBox childrenInRef childrenOutRef =
    unComponentSlot \p ctor input inputQuery outputQuery prjQuery -> do
      let ordP = mkOrdBox p
      childrenIn <- Ref.read childrenInRef
      var <- case M.pop ordP childrenIn of
        Just (Tuple existing childrenIn') -> do
          Ref.write childrenIn' childrenInRef
          for_ (inputQuery input) \q -> do
            dsx <- Ref.read existing
            unDriverStateX (\st -> for_ (st.prjQuery q) (handleAff <<< evalF st.selfRef <<< Query)) dsx
          pure existing
        Nothing ->
          runComponent lchs (maybe (pure unit) handler <<< outputQuery) input prjQuery ctor
      isDuplicate <- M.member ordP <$> Ref.read childrenOutRef
      when isDuplicate
        $ warn "Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"
      Ref.modify_ (M.insert ordP var) childrenOutRef
      Ref.read var >>= renderStateX case _ of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers
    :: forall f'
     . Ref LifecycleHandlers
    -> L.List (Aff Unit)
    -> DriverStateX h r f'
    -> Effect Unit
  squashChildInitializers lchs preInits =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <<< Query <$> st.component.initializer
      Ref.modify_ (\handlers ->
        { initializers: (do
            parSequence_ (L.reverse handlers.initializers)
            sequence_ parentInitializer
            liftEffect do
              handlePending st.pendingQueries
              handlePending st.pendingOuts) : preInits
        , finalizers: handlers.finalizers
        }) lchs

  handlePending
    :: Ref (Maybe (L.List (Aff Unit)))
    -> Effect Unit
  handlePending ref = do
    queue <- Ref.read ref
    Ref.write Nothing ref
    for_ queue (handleAff <<< traverse_ fork <<< L.reverse)

  cleanupSubscriptions
    :: forall s f' z' g p i' o'
     . DriverState h r s f' z' g p i' o'
    -> Effect Unit
  cleanupSubscriptions (DriverState ds) = do
    traverse_ (handleAff <<< traverse_ fork) =<< Ref.read ds.subscriptions
    Ref.write Nothing ds.subscriptions

  finalize
    :: forall f'
     . Ref LifecycleHandlers
    -> DriverStateX h r f'
    -> Effect Unit
  finalize lchs = do
    unDriverStateX \st -> do
      cleanupSubscriptions (DriverState st)
      for_ (evalF st.selfRef <<< Query <$> st.component.finalizer) \f ->
        Ref.modify_ (\handlers ->
          { initializers: handlers.initializers
          , finalizers: f : handlers.finalizers
          }) lchs
      for_ st.children (finalize lchs <=< Ref.read)

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall a
   . Aff a
  -> Effect Unit
handleAff = runAff_ (either throwException (const (pure unit)))
