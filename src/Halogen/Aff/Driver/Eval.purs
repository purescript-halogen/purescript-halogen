module Halogen.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, killFiber)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (Ref, modifyRef, modifyRef', readRef, writeRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork.Class (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence_, parallel, sequential)
import Data.Foldable (traverse_)
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), LifecycleHandlers, unDriverStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Query.EventSource as ES
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (ChildQueryBox, HalogenAp(..), HalogenF(..), HalogenM(..), unChildQuery)
import Halogen.Query.InputF (InputF(..), RefLabel(..))
import Unsafe.Reference (unsafeRefEq)

handleLifecycle
  :: forall eff a
   . Ref (LifecycleHandlers eff)
  -> Eff (HalogenEffects eff) a
  -> Aff (HalogenEffects eff) a
handleLifecycle lchs f = do
  liftEff $ writeRef lchs { initializers: L.Nil, finalizers: L.Nil }
  result <- liftEff f
  { initializers, finalizers } <- liftEff $ readRef lchs
  traverse_ fork finalizers
  parSequence_ initializers
  pure result

type Renderer h r eff
  = forall s f ps i o
   . Ref (LifecycleHandlers eff)
  -> Ref (DriverState h r s f ps i o eff)
  -> Eff (HalogenEffects eff) Unit

eval
  :: forall h r eff s f ps i o a
   . Renderer h r eff
  -> Ref (DriverState h r s f ps i o eff)
  -> InputF a (f a)
  -> Aff (HalogenEffects eff) a
eval render r =
  case _ of
    RefUpdate (RefLabel p) el next -> do
      liftEff $ modifyRef r \(DriverState st) ->
        DriverState st { refs = SM.alter (const el) p st.refs }
      pure next
    Query q -> evalF r q

  where

  go
    :: forall s' f' ps' i' o'
     . Ref (DriverState h r s' f' ps' i' o' eff)
    -> HalogenF s' f' ps' o' (Aff (HalogenEffects eff))
    ~> Aff (HalogenEffects eff)
  go ref = case _ of
    State f -> do
      DriverState (st@{ state, lifecycleHandlers }) <- liftEff (readRef ref)
      case f state of
        Tuple a state'
          | unsafeRefEq state state' -> pure a
          | otherwise -> do
              liftEff $ writeRef ref (DriverState (st { state = state' }))
              handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
              pure a
    Subscribe es next -> do
      DriverState ({ subscriptions, fresh }) <- liftEff (readRef ref)
      _ ← fork do
        { producer, done } <- ES.unEventSource es
        i <- liftEff $ modifyRef' fresh (\i -> { state: i + 1, value: i })
        let
          done' = do
            subs <- liftEff $ readRef subscriptions
            when (maybe false (M.member i) subs) do
              done
              liftEff $ modifyRef subscriptions (map (M.delete i))
        liftEff $ modifyRef subscriptions (map (M.insert i done'))
        let
          consumer = do
            q <- CR.await
            subs <- lift $ liftEff (readRef subscriptions)
            when (isJust subs) do
              s <- lift $ evalF ref q
              when (s == ES.Listening) consumer
        CR.runProcess (consumer `CR.pullFrom` producer)
        done'
      pure next
    Lift aff ->
      aff
    Halt msg ->
      throwError (error msg)
    ChildQuery cq ->
      evalChildQuery ref cq
    Raise o a -> do
      DriverState { handlerRef, pendingOuts } <- liftEff (readRef ref)
      handler <- liftEff (readRef handlerRef)
      queuingHandler handler pendingOuts o
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) -> do
        fiber <- fork (evalM ref fx)
        pure $ k (unsafeCoerceAff <<< flip killFiber fiber)) f
    GetRef (RefLabel p) k -> do
      DriverState { component, refs } <- liftEff (readRef ref)
      pure $ k $ SM.lookup p refs

  evalChildQuery
    :: forall s' f' ps' i' o' a'
     . Ref (DriverState h r s' f' ps' i' o' eff)
    -> ChildQueryBox ps' a'
    -> Aff (HalogenEffects eff) a'
  evalChildQuery ref cqb = do
    DriverState st <- liftEff (readRef ref)
    unChildQuery (\{ get, query, next } -> do
      case get st.children of
        Just (DriverStateRef var) -> do
          dsx <- liftEff (readRef var)
          unDriverStateX (\ds -> next <<< Just <$> evalF ds.selfRef query) dsx
        Nothing -> pure (next Nothing)) cqb

  evalF
    :: forall s' f' ps' i' o'
     . Ref (DriverState h r s' f' ps' i' o' eff)
    -> f'
    ~> Aff (HalogenEffects eff)
  evalF ref q = do
    DriverState st <- liftEff (readRef ref)
    case st.component.eval q of
      HalogenM fx -> foldFree (go ref) fx

  evalM
    :: forall s' f' ps' i' o'
     . Ref (DriverState h r s' f' ps' i' o' eff)
    -> HalogenM s' f' ps' o' (Aff (HalogenEffects eff))
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
