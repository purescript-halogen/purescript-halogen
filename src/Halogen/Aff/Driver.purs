module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  , module Halogen.Effects
  , module Exports
  ) where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Free (foldFree)
import Control.Monad.Fork (fork)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence_, sequential, parallel)

import Data.Lazy (force)
import Data.List (List, (:))
import Data.List as L
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple (Tuple(..))

import Halogen (HalogenIO)
import Halogen.Aff.Driver.State (ComponentType(..), DriverStateX, DriverState(..), unDriverStateX, initDriverState)
import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox, unOrdBox)
import Halogen.Effects (HalogenEffects)
import Halogen.Query.ChildQuery (ChildQuery, unChildQuery)
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))
import Halogen.Query.EventSource as ES

import Halogen.Aff.Driver.State (ComponentType(..)) as Exports

type LifecycleHandlers eff =
  { initializers :: List (Aff (HalogenEffects eff) Unit)
  , finalizers :: List (Aff (HalogenEffects eff) Unit)
  }

handleLifecycle
  :: forall eff r
   . (Ref (LifecycleHandlers eff) -> Aff (HalogenEffects eff) r)
  -> Aff (HalogenEffects eff) r
handleLifecycle f = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  result <- f lchs
  { initializers, finalizers } <- liftEff $ readRef lchs
  forkAll finalizers
  -- No need to par/fork initializers here as there's only ever zero or one at
  -- this point, due to the squashing at each level of the component hierarchy.
  sequence_ initializers
  pure result

type RenderSpec h r eff =
  { render
      :: forall f g p
       . (forall x. f x -> Eff (HalogenEffects eff) Unit)
      -> (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) r)
      -> h (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
      -> ComponentType
      -> Maybe r
      -> Eff (HalogenEffects eff) r
  , renderChild
      :: Int
      -> Maybe r
      -> Eff (HalogenEffects eff) r
  }

runUI
  :: forall h r f o eff
   . RenderSpec h r eff
  -> Component h f o (Aff (HalogenEffects eff))
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI renderSpec component = do
  fresh <- liftEff $ newRef 0
  handleLifecycle \lchs -> liftEff $ do
    listeners <- newRef M.empty
    runComponent (rootHandler listeners) fresh lchs Root component
      >>= readRef
      >>= unDriverStateX \st ->
        pure
          { query: evalF st.selfRef
          , subscribe: subscribe fresh listeners
          }

  where

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
    let producer = CR.producer (Left <$> AV.peekVar inputVar)
    void $ forkAff do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      AV.killVar inputVar (error "ended")

  runComponent
    :: forall f' o'
     . (o' -> Aff (HalogenEffects eff) Unit)
    -> Ref Int
    -> Ref (LifecycleHandlers eff)
    -> ComponentType
    -> Component h f' o' (Aff (HalogenEffects eff))
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f' eff))
  runComponent handler fresh lchs componentType = unComponent \c -> do
    keyId <- readRef fresh
    modifyRef fresh (_ + 1)
    var <- initDriverState c componentType handler keyId fresh
    unDriverStateX (render lchs <<< _.selfRef) =<< readRef var
    squashChildInitializers lchs =<< readRef var
    pure var

  eval
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> HalogenF s f' g p o' (Aff (HalogenEffects eff))
    ~> Aff (HalogenEffects eff)
  eval ref = case _ of
    GetState k -> do
      DriverState { state } <- liftEff (readRef ref)
      pure (k state)
    ModifyState f -> do
      DriverState (st@{ state }) <- liftEff (readRef ref)
      case f state of
        Tuple a state' -> do
          liftEff $ writeRef ref (DriverState (st { state = state' }))
          handleLifecycle \lchs -> liftEff $ render lchs ref
          pure a
    Subscribe es next -> do
      forkAff do
        { producer, done } <- ES.unEventSource es
        let
          consumer = do
            s <- lift <<< evalF ref =<< CR.await
            when (s == ES.Listening) consumer
        CR.runProcess (consumer `CR.pullFrom` producer)
        done
      pure next
    Lift aff ->
      aff
    Halt msg ->
      throwError (error msg)
    GetSlots k -> do
      DriverState { children } <- liftEff (readRef ref)
      pure $ k $ map unOrdBox $ M.keys children
    CheckSlot p k -> do
      DriverState { mkOrdBox, children } <- liftEff (readRef ref)
      pure $ k $ M.member (mkOrdBox p) children
    ChildQuery cq ->
      evalChildQuery ref cq
    Raise o a -> do
      DriverState (ds@{ handler, pendingOut }) <- liftEff (readRef ref)
      case pendingOut of
        Nothing -> do
          liftEff $ writeRef ref (DriverState ds)
          handler o
        Just p ->
          liftEff $ writeRef ref (DriverState ds { pendingOut = Just (o : p) })
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) →
        k <<< map unsafeCoerceAff <$> fork (evalM ref fx)) f

  evalChildQuery
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> ChildQuery g (Aff (HalogenEffects eff)) p
    ~> Aff (HalogenEffects eff)
  evalChildQuery ref = unChildQuery \p k -> do
    DriverState st <- liftEff (readRef ref)
    case M.lookup (st.mkOrdBox p) st.children of
      Just var -> do
        dsx <- liftEff (readRef var)
        k (unDriverStateX (\ds -> evalF ds.selfRef) dsx)
      Nothing -> throwError (error "Slot lookup failed for child query")

  evalF
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> f'
    ~> Aff (HalogenEffects eff)
  evalF ref q = do
    DriverState st <- liftEff (readRef ref)
    case st.component.eval q of
      HalogenM fx -> foldFree (eval ref) fx

  evalM
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> HalogenM s f' g p o' (Aff (HalogenEffects eff))
    ~> Aff (HalogenEffects eff)
  evalM ref (HalogenM q) = foldFree (eval ref) q

  render
    :: forall s f' g p o'
     . Ref (LifecycleHandlers eff)
    -> Ref (DriverState h r s f' g p o' eff)
    -> Eff (HalogenEffects eff) Unit
  render lchs var = readRef var >>= \(DriverState ds) -> do
    childrenVar <- newRef M.empty
    oldChildren <- newRef ds.children
    let
      selfEval = evalF ds.selfRef
      handler :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler = void <<< selfEval
      handler' :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler' = maybe handler (\_ -> queuingHandler ds.selfRef handler) ds.pendingIn
    rendering <-
      renderSpec.render
        (handleAff <<< selfEval)
        (renderChild handler' ds.fresh ds.mkOrdBox oldChildren childrenVar lchs)
        (ds.component.render ds.state)
        ds.componentType
        ds.rendering
    children <- readRef childrenVar
    traverse_ (addFinalizer lchs <=< readRef) =<< readRef oldChildren
    writeRef var $
      DriverState
        { rendering: Just rendering
        , componentType: ds.componentType
        , component: ds.component
        , state: ds.state
        , children
        , mkOrdBox: ds.mkOrdBox
        , selfRef: ds.selfRef
        , handler: ds.handler
        , pendingIn: ds.pendingIn
        , pendingOut: ds.pendingOut
        , keyId: ds.keyId
        , fresh: ds.fresh
        }

  queuingHandler
    :: forall s f' g p o' x
     . Ref (DriverState h r s f' g p o' eff)
    -> (f' x -> Aff (HalogenEffects eff) Unit)
    -> f' x
    -> Aff (HalogenEffects eff) Unit
  queuingHandler var handler message = do
    DriverState (ds@{ pendingIn }) <- liftEff (readRef var)
    case pendingIn of
      Nothing -> do
        liftEff $ writeRef var (DriverState ds)
        handler message
      Just p ->
        liftEff $ writeRef var (DriverState ds { pendingIn = Just (handler message : p) })

  renderChild
    :: forall f' g p
     . (forall x. f' x -> Aff (HalogenEffects eff) Unit)
    -> Ref Int
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> Ref (LifecycleHandlers eff)
    -> ComponentSlot h g (Aff (HalogenEffects eff)) p (f' Unit)
    -> Eff (HalogenEffects eff) r
  renderChild handler fresh mkOrdBox childrenInRef childrenOutRef lchs =
    unComponentSlot \p ctor k -> do
      childrenIn <- readRef childrenInRef
      var <- case M.pop (mkOrdBox p) childrenIn of
        Just (Tuple existing childrenIn') -> do
          writeRef childrenInRef childrenIn'
          pure existing
        Nothing ->
          runComponent (maybe (pure unit) handler <<< k) fresh lchs Child (force ctor)
      modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
      unDriverStateX (\st -> renderSpec.renderChild st.keyId st.rendering) =<< readRef var

  addInitializer
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX h r f' eff
    -> Aff (HalogenEffects eff) Unit
  addInitializer ref =
    unDriverStateX \st -> do
      case evalF st.selfRef <$> st.component.initializer of
        Just i -> do
          liftEff $ modifyRef ref \lchs ->
            { initializers: handlePending st.selfRef : i : lchs.initializers
            , finalizers: lchs.finalizers
            }
        _ ->
          pure unit

  squashChildInitializers
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  squashChildInitializers ref =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <$> st.component.initializer
      modifyRef ref \lchs ->
        { initializers: pure $ do
            parSequence_ (L.reverse lchs.initializers)
            sequence_ parentInitializer
            handlePending st.selfRef
        , finalizers: lchs.finalizers
        }

  handlePending
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> Aff (HalogenEffects eff) Unit
  handlePending ref = do
    DriverState (dsi@{ pendingIn }) <- liftEff (readRef ref)
    liftEff $ writeRef ref (DriverState dsi { pendingIn = Nothing })
    for_ pendingIn (forkAll <<< L.reverse)
    DriverState (dso@{ pendingOut, handler }) <- liftEff (readRef ref)
    liftEff $ writeRef ref (DriverState dso { pendingOut = Nothing })
    for_ pendingOut (forkAll <<< map handler <<< L.reverse)

  addFinalizer
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  addFinalizer ref =
    unDriverStateX \st -> do
      for_ (evalF st.selfRef <$> st.component.finalizer) \f ->
        modifyRef ref (\lchs ->
          { initializers: lchs.initializers
          , finalizers: f : lchs.finalizers
          })
      for_ st.children (addFinalizer ref <=< readRef)

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall eff a
   . Aff (HalogenEffects eff) a
  -> Eff (HalogenEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))
