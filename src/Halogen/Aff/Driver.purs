module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  , module Halogen.Aff.Effects
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (forkAll)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error, throw, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (class Parallel, parSequence_)

import Data.List ((:))
import Data.List as L
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple (Tuple(..))

import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval (eval, handleLifecycle, queuingHandler)
import Halogen.Aff.Driver.State (LifecycleHandlers, DriverState(..), DriverStateX, RenderStateX, initDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox)
import Halogen.Query.InputF (InputF(..))

type RenderSpec h r m eff =
  { render
      :: forall s f g p o
       . (forall x. InputF x (f x) -> Eff (HalogenEffects eff) Unit)
      -> (ComponentSlot h g m p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX r m))
      -> h (ComponentSlot h g m p (f Unit)) (f Unit)
      -> Maybe (r s f g p o m)
      -> Eff (HalogenEffects eff) (r s f g p o m)
  , renderChild :: forall s f g p o. r s f g p o m -> r s f g p o m
  , removeChild :: forall s f g p o. r s f g p o m -> Eff (HalogenEffects eff) Unit
  }

newLifecycleHandlers :: forall m eff. Eff (HalogenEffects eff) (Ref (LifecycleHandlers m))
newLifecycleHandlers = newRef { initializers: L.Nil, finalizers: L.Nil }

runUI
  :: forall h r f i o m eff t
   . (MonadAff (HalogenEffects eff) m, Parallel t m, MonadFork Error m, MonadRec m, MonadError Error m)
  => RenderSpec h r m eff
  -> Component h f i o m
  -> i
  -> m (HalogenIO f o m)
runUI renderSpec component i = do
  emitterRef <- liftEff $ newRef (\_ -> pure unit)
  let
    producer = CRA.produce' \emit -> writeRef emitterRef (emit <<< Left)
    consumer = forever (lift =<< CR.await)
  fork $ CR.runProcess (CR.connect producer consumer)
  emitter <- liftEff $ readRef emitterRef
  runUI' renderSpec emitter component i

runUI'
  :: forall h r f i o m eff t
   . (MonadAff (HalogenEffects eff) m, Parallel t m, MonadFork Error m, MonadRec m, MonadError Error m)
  => RenderSpec h r m eff
  -> (m Unit -> Eff (HalogenEffects eff) Unit)
  -> Component h f i o m
  -> i
  -> m (HalogenIO f o m)
runUI' renderSpec handleM component i = do
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
     . Ref (DriverState h r s f' z' g p i' o' m)
    -> (forall x. f' x -> Maybe (z' x))
    -> f'
    ~> m
  evalDriver ref prjQuery q =
    case prjQuery q of
      Just q' -> evalF ref (Query q')
      Nothing -> liftEff $ throwException (error "Halogen internal error: query projection failed in runUI'")

  evalF
    :: forall s f' z' g p i' o' a
     . Ref (DriverState h r s f' z' g p i' o' m)
    -> InputF a (z' a)
    -> m a
  evalF ref = eval render ref

  rootHandler
    :: Ref (M.Map Int (AV.AVar o))
    -> o
    -> m Unit
  rootHandler ref message = liftAff do
    listeners <- liftEff $ readRef ref
    void $ forkAll $ map (\var -> AV.putVar var message) listeners

  subscribe
    :: Ref Int
    -> Ref (M.Map Int (AV.AVar o))
    -> CR.Consumer o m Unit
    -> m Unit
  subscribe fresh ref consumer = do
    inputVar <- liftAff AV.makeVar
    listenerId <- liftEff do
      listenerId <- readRef fresh
      modifyRef fresh (_ + 1)
      modifyRef ref (M.insert listenerId inputVar)
      pure listenerId
    let producer = CR.producer (Left <$> liftAff (AV.takeVar inputVar))
    void $ fork do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      liftAff $ AV.killVar inputVar (error "ended")

  runComponent
    :: forall z f' i' o'
     . Ref (LifecycleHandlers m)
    -> (o' -> m Unit)
    -> i'
    -> (forall x. f' x -> Maybe (z x))
    -> Component h z i' o' m
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f' m))
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
     . Ref (LifecycleHandlers m)
    -> Ref (DriverState h r s f' z' g p i' o' m)
    -> Eff (HalogenEffects eff) Unit
  render lchs var = readRef var >>= \(DriverState ds) -> do
    writeRef ds.childrenOut M.empty
    writeRef ds.childrenIn ds.children
    let
      handler :: forall x. InputF x (z' x) -> m Unit
      handler = void <<< evalF ds.selfRef
      childHandler :: forall x. z' x -> m Unit
      childHandler = queuingHandler (handler <<< Query) ds.pendingQueries
    rendering <-
      renderSpec.render
        (handleM <<< handler)
        (renderChild lchs childHandler ds.component.mkOrdBox ds.childrenIn ds.childrenOut)
        (ds.component.render ds.state)
        ds.rendering
    children <- readRef ds.childrenOut
    readRef ds.childrenIn >>= traverse_ \childVar -> do
      childDS <- readRef childVar
      renderStateX_ renderSpec.removeChild childDS
      cleanupSubscriptions childDS
      addFinalizer lchs childDS
    modifyRef var \(DriverState ds') ->
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
        , prjQuery: ds'.prjQuery
        , fresh: ds'.fresh
        , subscriptions: ds'.subscriptions
        , lifecycleHandlers: ds'.lifecycleHandlers
        }

  renderChild
    :: forall f' g p
     . Ref (LifecycleHandlers m)
    -> (forall x. f' x -> m Unit)
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g m)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g m)))
    -> ComponentSlot h g m p (f' Unit)
    -> Eff (HalogenEffects eff) (RenderStateX r m)
  renderChild lchs handler mkOrdBox childrenInRef childrenOutRef =
    unComponentSlot \p ctor input inputQuery outputQuery prjQuery -> do
      childrenIn <- readRef childrenInRef
      var <- case M.pop (mkOrdBox p) childrenIn of
        Just (Tuple existing childrenIn') -> do
          writeRef childrenInRef childrenIn'
          for_ (inputQuery input) \q -> do
            dsx <- readRef existing
            unDriverStateX (\st -> for_ (st.prjQuery q) (handleM <<< evalF st.selfRef <<< Query)) dsx
          pure existing
        Nothing ->
          runComponent lchs (maybe (pure unit) handler <<< outputQuery) input prjQuery ctor
      modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
      readRef var >>= renderStateX case _ of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers
    :: forall f'
     . Ref (LifecycleHandlers m)
    -> L.List (m Unit)
    -> DriverStateX h r f' m
    -> Eff (HalogenEffects eff) Unit
  squashChildInitializers lchs preInits =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <<< Query <$> st.component.initializer
      modifyRef lchs \handlers ->
        { initializers: (do
            parSequence_ (L.reverse handlers.initializers)
            sequence_ parentInitializer
            liftEff do
              handlePending st.pendingQueries
              handlePending st.pendingOuts) : preInits
        , finalizers: handlers.finalizers
        }

  handlePending
    :: Ref (Maybe (L.List (m Unit)))
    -> Eff (HalogenEffects eff) Unit
  handlePending ref = do
    queue <- readRef ref
    writeRef ref Nothing
    for_ queue (handleM <<< void <<< fork <<< parSequence_ <<< L.reverse)

  cleanupSubscriptions
    :: forall f'
     . DriverStateX h r f' m
    -> Eff (HalogenEffects eff) Unit
  cleanupSubscriptions = unDriverStateX \ds -> do
    traverse_ (handleM <<< void <<< fork <<< parSequence_) =<< readRef ds.subscriptions
    writeRef ds.subscriptions Nothing

  addFinalizer
    :: forall f'
     . Ref (LifecycleHandlers m)
    -> DriverStateX h r f' m
    -> Eff (HalogenEffects eff) Unit
  addFinalizer lchs =
    unDriverStateX \st -> do
      for_ (evalF st.selfRef <<< Query <$> st.component.finalizer) \f ->
        modifyRef lchs (\handlers ->
          { initializers: handlers.initializers
          , finalizers: f : handlers.finalizers
          })
      for_ st.children (addFinalizer lchs <=< readRef)
