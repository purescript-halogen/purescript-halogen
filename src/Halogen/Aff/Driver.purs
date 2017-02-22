module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  , module Halogen.Aff.Effects
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throw, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Monad.Rec.Class (Step(..), tailRecM)

import Data.List ((:))
import Data.List as L
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple (Tuple(..))

import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval (eval, handleLifecycle, queuingHandler, parSequenceAff_)
import Halogen.Aff.Driver.State (LifecycleHandlers, DriverState(..), DriverStateX, RenderStateX, initDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox)
import Halogen.Query.InputF (InputF(..))

type RenderSpec h r eff =
  { render
      :: forall s f g p o
       . (forall x. InputF x (f x) -> Eff (HalogenEffects eff) Unit)
      -> (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX r eff))
      -> h (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
      -> Maybe (r s f g p o eff)
      -> Eff (HalogenEffects eff) (r s f g p o eff)
  , renderChild :: forall s f g p o. r s f g p o eff -> r s f g p o eff
  , removeChild :: forall s f g p o. r s f g p o eff -> Eff (HalogenEffects eff) Unit
  }

newLifecycleHandlers :: forall eff. Eff (HalogenEffects eff) (Ref (LifecycleHandlers eff))
newLifecycleHandlers = newRef { initializers: L.Nil, finalizers: L.Nil }

runUI
  :: forall h r f i o eff
   . RenderSpec h r eff
  -> Component h f i o (Aff (HalogenEffects eff))
  -> i
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI renderSpec component i = do
  lchs <- liftEff newLifecycleHandlers
  fresh <- liftEff $ newRef 0
  handleLifecycle lchs do
    listeners <- newRef M.empty
    runComponent lchs (rootHandler listeners) i Just component
      >>= readRef
      >>= unDriverStateX \st ->
        pure
          { query: evalDriver st.selfRef st.prjQuery
          , subscribe: subscribe fresh listeners
          }

  where

  evalDriver
    :: forall s f' z' g p i' o'
     . Ref (DriverState h r s f' z' g p i' o' eff)
    -> (forall x. f' x -> Maybe (z' x))
    -> f'
    ~> Aff (HalogenEffects eff)
  evalDriver ref prjQuery q =
    case prjQuery q of
      Just q' -> evalF ref (Query q')
      Nothing -> liftEff $ throwException (error "Halogen internal error: query projection failed in runUI'")

  evalF
    :: forall s f' z' g p i' o' a
     . Ref (DriverState h r s f' z' g p i' o' eff)
    -> InputF a (z' a)
    -> Aff (HalogenEffects eff) a
  evalF ref = eval render ref

  rootHandler
    :: Ref (M.Map Int (AV.AVar o))
    -> o
    -> Aff (HalogenEffects eff) Unit
  rootHandler ref message = do
    listeners <- liftEff $ readRef ref
    void $ forkAll $ map (\var -> AV.putVar var message) listeners

  subscribe
    :: Ref Int
    -> Ref (M.Map Int (AV.AVar o))
    -> CR.Consumer o (Aff (HalogenEffects eff)) Unit
    -> Aff (HalogenEffects eff) Unit
  subscribe fresh ref consumer = do
    inputVar <- AV.makeVar
    listenerId <- liftEff do
      listenerId <- readRef fresh
      modifyRef fresh (_ + 1)
      modifyRef ref (M.insert listenerId inputVar)
      pure listenerId
    let producer = CR.producer (Left <$> AV.takeVar inputVar)
    void $ forkAff do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      AV.killVar inputVar (error "ended")

  runComponent
    :: forall z f' i' o'
     . Ref (LifecycleHandlers eff)
    -> (o' -> Aff (HalogenEffects eff) Unit)
    -> i'
    -> (forall x. f' x -> Maybe (z x))
    -> Component h z i' o' (Aff (HalogenEffects eff))
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f' eff))
  runComponent lchs handler j prjQuery = unComponent \c -> do
    lchs' <- newLifecycleHandlers
    var <- initDriverState c j handler prjQuery lchs'
    pre <- readRef lchs
    writeRef lchs { initializers: L.Nil, finalizers: pre.finalizers }
    unDriverStateX (render lchs <<< _.selfRef) =<< readRef var
    squashChildInitializers lchs pre.initializers =<< readRef var
    pure var

  render
    :: forall s f' z' g p i' o'
     . Ref (LifecycleHandlers eff)
    -> Ref (DriverState h r s f' z' g p i' o' eff)
    -> Eff (HalogenEffects eff) Unit
  render lchs var = readRef var >>= \(DriverState ds) -> do
    shouldProcessHandlers <- isNothing <$> readRef ds.pendingHandlers
    when shouldProcessHandlers $ writeRef ds.pendingHandlers (Just L.Nil)
    writeRef ds.childrenOut M.empty
    writeRef ds.childrenIn ds.children
    let
      handler :: forall x. InputF x (z' x) -> Aff (HalogenEffects eff) Unit
      handler = queuingHandler (void <<< evalF ds.selfRef) ds.pendingHandlers
      childHandler :: forall x. z' x -> Aff (HalogenEffects eff) Unit
      childHandler = queuingHandler (handler <<< Query) ds.pendingQueries
    rendering <-
      renderSpec.render
        (handleAff <<< handler)
        (renderChild lchs childHandler ds.component.mkOrdBox ds.childrenIn ds.childrenOut)
        (ds.component.render ds.state)
        ds.rendering
    children <- readRef ds.childrenOut
    readRef ds.childrenIn >>= traverse_ \childVar -> do
      childDS <- readRef childVar
      renderStateX_ renderSpec.removeChild childDS
      cleanupSubscriptions childDS
      addFinalizer lchs childDS
    modifyRef ds.selfRef \(DriverState ds') ->
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
        }
    when shouldProcessHandlers do
      flip tailRecM unit \_ -> do
        handlers <- readRef ds.pendingHandlers
        writeRef ds.pendingHandlers (Just L.Nil)
        maybe (pure unit) (handleAff <<< forkAll <<< L.reverse) handlers
        mmore <- readRef ds.pendingHandlers
        if maybe false L.null mmore
          then writeRef ds.pendingHandlers Nothing $> Done unit
          else pure $ Loop unit

  renderChild
    :: forall f' g p
     . Ref (LifecycleHandlers eff)
    -> (forall x. f' x -> Aff (HalogenEffects eff) Unit)
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> ComponentSlot h g (Aff (HalogenEffects eff)) p (f' Unit)
    -> Eff (HalogenEffects eff) (RenderStateX r eff)
  renderChild lchs handler mkOrdBox childrenInRef childrenOutRef =
    unComponentSlot \p ctor input inputQuery outputQuery prjQuery -> do
      childrenIn <- readRef childrenInRef
      var <- case M.pop (mkOrdBox p) childrenIn of
        Just (Tuple existing childrenIn') -> do
          writeRef childrenInRef childrenIn'
          for_ (inputQuery input) \q -> do
            dsx <- readRef existing
            unDriverStateX (\st -> for_ (st.prjQuery q) (handleAff <<< evalF st.selfRef <<< Query)) dsx
          pure existing
        Nothing ->
          runComponent lchs (maybe (pure unit) handler <<< outputQuery) input prjQuery ctor
      modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
      readRef var >>= renderStateX case _ of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> L.List (Aff (HalogenEffects eff) Unit)
    -> DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  squashChildInitializers lchs preInits =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <<< Query <$> st.component.initializer
      modifyRef lchs \handlers ->
        { initializers: (do
            parSequenceAff_ (L.reverse handlers.initializers)
            sequence_ parentInitializer
            liftEff do
              handlePending st.pendingQueries
              handlePending st.pendingOuts) : preInits
        , finalizers: handlers.finalizers
        }

  handlePending
    :: Ref (Maybe (L.List (Aff (HalogenEffects eff) Unit)))
    -> Eff (HalogenEffects eff) Unit
  handlePending ref = do
    queue <- readRef ref
    writeRef ref Nothing
    for_ queue (handleAff <<< forkAll <<< L.reverse)

  cleanupSubscriptions
    :: forall f'
     . DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  cleanupSubscriptions = unDriverStateX \ds -> do
    traverse_ (handleAff <<< forkAll) =<< readRef ds.subscriptions
    writeRef ds.subscriptions Nothing

  addFinalizer
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  addFinalizer lchs =
    unDriverStateX \st -> do
      for_ (evalF st.selfRef <<< Query <$> st.component.finalizer) \f ->
        modifyRef lchs (\handlers ->
          { initializers: handlers.initializers
          , finalizers: f : handlers.finalizers
          })
      for_ st.children (addFinalizer lchs <=< readRef)

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall eff a
   . Aff (HalogenEffects eff) a
  -> Eff (HalogenEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))
