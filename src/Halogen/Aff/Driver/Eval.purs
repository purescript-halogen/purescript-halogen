module Halogen.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (Ref, readRef, modifyRef, writeRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)

import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence_)
import Data.Tuple (Tuple(..))

import Halogen.Aff.Driver.State (DriverState(..), unDriverStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Data.OrdBox (unOrdBox)
import Halogen.Query.ChildQuery (ChildQuery, unChildQuery)
import Halogen.Query.EventSource as ES
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))
import Halogen.Query.InputF (InputF(..))

type LifecycleHandlers eff =
  { initializers :: List (Aff (HalogenEffects eff) Unit)
  , finalizers :: List (Aff (HalogenEffects eff) Unit)
  }

handleLifecycle
  :: forall eff a
   . Ref (LifecycleHandlers eff)
  -> Eff (HalogenEffects eff) a
  -> Aff (HalogenEffects eff) a
handleLifecycle lchs f = do
  liftEff $ writeRef lchs { initializers: L.Nil, finalizers: L.Nil }
  result <- liftEff f
  { initializers, finalizers } <- liftEff $ readRef lchs
  forkAll finalizers
  -- No need to par/fork initializers here as there's only ever zero or one at
  -- this point, due to the squashing at each level of the component hierarchy.
  sequence_ initializers
  pure result

type Renderer h r eff
  = forall s f z g p i o
   . Ref (DriverState h r s f z g p i o eff)
  -> Eff (HalogenEffects eff) Unit

eval
  :: forall h r eff s'' f z g'' p'' i o a
   . Ref (LifecycleHandlers eff)
  -> Renderer h r eff
  -> Ref (DriverState h r s'' f z g'' p'' i o eff)
  -> InputF p'' a (z a)
  -> Aff (HalogenEffects eff) a
eval lchs render r =
  case _ of
    RefUpdate p el next -> do
      liftEff $ modifyRef r \(DriverState st) ->
        DriverState st { refs = M.alter (const el) (st.component.mkOrdBox p) st.refs }
      pure next
    Query q -> evalF r q

  where

  go
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' eff)
    -> HalogenF s' z' g' p' o' (Aff (HalogenEffects eff))
    ~> Aff (HalogenEffects eff)
  go ref = case _ of
    GetState k -> do
      DriverState { state } <- liftEff (readRef ref)
      pure (k state)
    ModifyState f -> do
      DriverState (st@{ state }) <- liftEff (readRef ref)
      case f state of
        Tuple a state' -> do
          liftEff $ writeRef ref (DriverState (st { state = state' }))
          handleLifecycle lchs (render ref)
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
      DriverState { component, children } <- liftEff (readRef ref)
      pure $ k $ M.member (component.mkOrdBox p) children
    ChildQuery cq ->
      evalChildQuery ref cq
    Raise o a -> do
      DriverState { handler, pendingOuts } <- liftEff (readRef ref)
      queuingHandler handler pendingOuts o
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) →
        k <<< map unsafeCoerceAff <$> fork (evalM ref fx)) f
    GetRef p k -> do
      DriverState { component, refs } <- liftEff (readRef ref)
      pure $ k $ M.lookup (component.mkOrdBox p) refs

  evalChildQuery
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' eff)
    -> ChildQuery g' (Aff (HalogenEffects eff)) p'
    ~> Aff (HalogenEffects eff)
  evalChildQuery ref = unChildQuery \p k -> do
    DriverState st <- liftEff (readRef ref)
    case M.lookup (st.component.mkOrdBox p) st.children of
      Just var -> do
        dsx <- liftEff (readRef var)
        k (unDriverStateX (\ds q -> case ds.prjQuery q of
            Just q' -> evalF ds.selfRef q'
            Nothing -> throwError (error "Query projection failed for child query")) dsx)
      Nothing -> throwError (error "Slot lookup failed for child query")

  evalF
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' eff)
    -> z'
    ~> Aff (HalogenEffects eff)
  evalF ref q = do
    DriverState st <- liftEff (readRef ref)
    case st.component.eval q of
      HalogenM fx -> foldFree (go ref) fx

  evalM
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' eff)
    -> HalogenM s' z' g' p' o' (Aff (HalogenEffects eff))
    ~> Aff (HalogenEffects eff)
  evalM ref (HalogenM q) = foldFree (go ref) q

queuingHandler
  :: forall a eff
   . (a -> Aff (HalogenEffects eff) Unit)
  -> Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  -> a
  -> Aff (HalogenEffects eff) Unit
queuingHandler handler ref message = do
  queue <- liftEff (readRef ref)
  case queue of
    Nothing ->
      handler message
    Just p ->
      liftEff $ writeRef ref (Just (handler message : p))
