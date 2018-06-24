module Halogen.Aff.Driver.Eval
  ( Renderer
  , evalF
  , evalQ
  , evalM
  , handleLifecycle
  , queueOrRun
  ) where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Fork.Class (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence_, parallel, sequential)
import Data.Foldable (traverse_)
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, killFiber)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), LifecycleHandlers, mapDriverState, unDriverStateX)
import Halogen.Query.EventSource as ES
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (HalogenAp(..), HalogenF(..), HalogenM'(..), ChildQuery, UnpackQuery(..), SubscriptionId(..), unChildQuery)
import Halogen.Query.HalogenQ (HalogenQ(..))
import Halogen.Query.Input (Input(..), RefLabel(..))
import Unsafe.Reference (unsafeRefEq)

type Renderer h r
  = forall s f act ps i o
   . Ref LifecycleHandlers
  -> Ref (DriverState h r s f act ps i o)
  -> Effect Unit

evalF
  :: forall h r s f act ps i o
   . Renderer h r
  -> Ref (DriverState h r s f act ps i o)
  -> Input act
  -> Aff Unit
evalF render ref = case _ of
  RefUpdate (RefLabel p) el -> do
    liftEffect $ flip Ref.modify_ ref $ mapDriverState \st ->
      st { refs = M.alter (const el) p st.refs }
  Action act -> do
    DriverState st <- liftEffect (Ref.read ref)
    evalM render ref (st.component.eval (Handle act unit))

evalQ
  :: forall h r s f act ps i o
   . Renderer h r
  -> Ref (DriverState h r s f act ps i o)
  -> f
  ~> Aff
evalQ render ref q = do
  DriverState st <- liftEffect (Ref.read ref)
  evalM render ref (st.component.eval (Request q))

evalM
  :: forall h r s f act ps i o
   . Renderer h r
  -> Ref (DriverState h r s f act ps i o)
  -> HalogenM' s act ps o Aff
  ~> Aff
evalM render initRef (HalogenM hm) = foldFree (go initRef) hm
  where
  go
    :: forall s' f' act' ps' i' o'
     . Ref (DriverState h r s' f' act' ps' i' o')
    -> HalogenF s' act' ps' o' Aff
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
    Subscribe fes k -> do
      DriverState { fresh } <- liftEffect (Ref.read ref)
      sid <- liftEffect $ Ref.modify' (\i -> { state: i + 1, value: SubscriptionId i }) fresh
      let (ES.EventSource setup) = fes sid
      DriverState ({ subscriptions }) <- liftEffect (Ref.read ref)
      _ ← fork do
        { producer, finalizer } <- setup
        let
          done = ES.Finalizer do
            subs <- liftEffect $ Ref.read subscriptions
            liftEffect $ Ref.modify_ (map (M.delete sid)) subscriptions
            when (maybe false (M.member sid) subs) (ES.finalize finalizer)
          consumer = do
            act <- CR.await
            subs <- lift $ liftEffect (Ref.read subscriptions)
            when ((M.member sid <$> subs) == Just true) do
              _ <- lift $ fork $ evalF render ref (Action act)
              consumer
        liftEffect $ Ref.modify_ (map (M.insert sid done)) subscriptions
        CR.runProcess (consumer `CR.pullFrom` producer)
        ES.finalize done
      pure (k sid)
    Unsubscribe sid next -> do
      unsubscribe sid ref
      pure next
    Lift aff ->
      aff
    ChildQuery cq ->
      evalChildQuery ref cq
    Raise o a -> do
      DriverState { handlerRef, pendingOuts } <- liftEffect (Ref.read ref)
      handler <- liftEffect (Ref.read handlerRef)
      queueOrRun pendingOuts (handler o)
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM render ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) -> do
        fiber <- fork (evalM render ref fx)
        pure $ k (flip killFiber fiber)) f
    GetRef (RefLabel p) k -> do
      DriverState { component, refs } <- liftEffect (Ref.read ref)
      pure $ k $ M.lookup p refs

  evalChildQuery
    :: forall s' f' act' ps' i' o' a'
     . Ref (DriverState h r s' f' act' ps' i' o')
    -> ChildQuery ps' a'
    -> Aff a'
  evalChildQuery ref cqb = do
    DriverState st <- liftEffect (Ref.read ref)
    unChildQuery (\{ unpack: UnpackQuery unpack, query, reply } -> do
      let
        evalChild (DriverStateRef var) = parallel do
          dsx <- liftEffect (Ref.read var)
          unDriverStateX (\ds -> evalQ render ds.selfRef query) dsx
      reply <$> sequential (unpack evalChild st.children)) cqb

unsubscribe
  :: forall h r s' f' act' ps' i' o'
   . SubscriptionId
  -> Ref (DriverState h r s' f' act' ps' i' o')
  -> Aff Unit
unsubscribe sid ref = do
  DriverState ({ subscriptions }) <- liftEffect (Ref.read ref)
  subs <- liftEffect (Ref.read subscriptions)
  traverse_ ES.finalize (M.lookup sid =<< subs)

handleLifecycle :: Ref LifecycleHandlers -> Effect ~> Aff
handleLifecycle lchs f = do
  liftEffect $ Ref.write { initializers: L.Nil, finalizers: L.Nil } lchs
  result <- liftEffect f
  { initializers, finalizers } <- liftEffect $ Ref.read lchs
  traverse_ fork finalizers
  parSequence_ initializers
  pure result

queueOrRun
  :: Ref (Maybe (List (Aff Unit)))
  -> Aff Unit
  -> Aff Unit
queueOrRun ref au =
  liftEffect (Ref.read ref) >>= case _ of
    Nothing -> au
    Just p -> liftEffect $ Ref.write (Just (au : p)) ref
