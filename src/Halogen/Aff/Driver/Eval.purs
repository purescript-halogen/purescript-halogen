module Halogen.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork.Class (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence_, parallel, sequential)
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Foldable (traverse_)
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.State (DriverState(..), LifecycleHandlers, unDriverStateX)
import Halogen.Data.OrdBox (unOrdBox)
import Halogen.Query.EventSource as ES
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))
import Halogen.Query.InputF (InputF(..), RefLabel(..))
import Unsafe.Reference (unsafeRefEq)

handleLifecycle
  :: forall a
   . Ref LifecycleHandlers
  -> Effect a
  -> Aff a
handleLifecycle lchs f = do
  liftEffect $ Ref.write { initializers: L.Nil, finalizers: L.Nil } lchs
  result <- liftEffect f
  { initializers, finalizers } <- liftEffect $ Ref.read lchs
  traverse_ fork finalizers
  parSequence_ initializers
  pure result

type Renderer h r
  = forall s f z g p i o
   . Ref LifecycleHandlers
  -> Ref (DriverState h r s f z g p i o)
  -> Effect Unit

eval
  :: forall h r s'' f z g'' p'' i o a
   . Renderer h r
  -> Ref (DriverState h r s'' f z g'' p'' i o)
  -> InputF a (z a)
  -> Aff a
eval render r =
  case _ of
    RefUpdate (RefLabel p) el next -> do
      liftEffect $ Ref.modify_ (\(DriverState st) ->
        DriverState st { refs = M.alter (const el) p st.refs }) r
      pure next
    Query q -> evalF r q

  where

  go
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o')
    -> HalogenF s' z' g' p' o' Aff
    ~> Aff
  go ref = case _ of
    State f -> do
      DriverState (st@{ state, lifecycleHandlers }) <- liftEffect (Ref.read ref)
      case f state of
        Tuple a state'
          | unsafeRefEq state state' -> pure a
          | otherwise -> do
              liftEffect $ Ref.write (DriverState (st { state = state' })) ref
              handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
              pure a
    Subscribe es next -> do
      DriverState ({ subscriptions, fresh }) <- liftEffect (Ref.read ref)
      _ ← fork do
        { producer, done } <- ES.unEventSource es
        i <- liftEffect $ Ref.modify' (\i -> { state: i + 1, value: i }) fresh
        let
          done' = do
            subs <- liftEffect $ Ref.read subscriptions
            when (maybe false (M.member i) subs) do
              liftEffect $ Ref.modify_ (map (M.delete i)) subscriptions
        liftEffect $ Ref.modify_ (map (M.insert i done')) subscriptions
        let
          consumer = do
            q <- CR.await
            subs <- lift $ liftEffect (Ref.read subscriptions)
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
    GetSlots k -> do
      DriverState { children } <- liftEffect (Ref.read ref)
      pure $ k $ map unOrdBox $ L.fromFoldable $ M.keys children
    CheckSlot p k -> do
      DriverState { component, children } <- liftEffect (Ref.read ref)
      pure $ k $ M.member (component.mkOrdBox p) children
    ChildQuery p cq ->
      evalChildQuery ref p cq
    Raise o a -> do
      DriverState { handler, pendingOuts } <- liftEffect (Ref.read ref)
      queuingHandler handler pendingOuts o
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) -> do
        fiber <- fork (evalM ref fx)
        pure $ k (flip killFiber fiber)) f
    GetRef (RefLabel p) k -> do
      DriverState { component, refs } <- liftEffect (Ref.read ref)
      pure $ k $ M.lookup p refs

  evalChildQuery
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o')
    -> p'
    -> Coyoneda g'
    ~> Aff
  evalChildQuery ref p = unCoyoneda \k q -> do
    DriverState st <- liftEffect (Ref.read ref)
    case M.lookup (st.component.mkOrdBox p) st.children of
      Just var -> do
        dsx <- liftEffect (Ref.read var)
        unDriverStateX (\ds -> case ds.prjQuery q of
          Just q' -> k <$> evalF ds.selfRef q'
          Nothing -> throwError (error "Query projection failed for child query")) dsx
      Nothing -> throwError (error "Slot lookup failed for child query")

  evalF
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o')
    -> z'
    ~> Aff
  evalF ref q = do
    DriverState st <- liftEffect (Ref.read ref)
    case st.component.eval q of
      HalogenM fx -> foldFree (go ref) fx

  evalM
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o')
    -> HalogenM s' z' g' p' o' Aff
    ~> Aff
  evalM ref (HalogenM q) = foldFree (go ref) q

queuingHandler
  :: forall a
   . (a -> Aff Unit)
  -> Ref (Maybe (List (Aff Unit)))
  -> a
  -> Aff Unit
queuingHandler handler ref message = do
  queue <- liftEffect (Ref.read ref)
  case queue of
    Nothing ->
      handler message
    Just p ->
      liftEffect $ Ref.write (Just (handler message : p)) ref
