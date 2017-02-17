module Halogen.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, attempt, forkAll)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Ref (REF, Ref, readRef, modifyRef, modifyRef', writeRef, newRef)
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Free.Trans (runFreeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (class Parallel, parSequence_, parallel, sequential)

import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Either (Either(..))
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), isJust)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))

import Halogen.Aff.Driver.State (DriverState(..), LifecycleHandlers, unDriverStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Data.OrdBox (unOrdBox)
import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))
import Halogen.Query.InputF (InputF(..), RefLabel(..))

parSequenceAff_
  :: forall eff a
   . L.List (Aff (avar :: AV.AVAR, ref :: REF | eff) a)
  -> Aff (avar :: AV.AVAR, ref :: REF | eff) Unit
parSequenceAff_ = case _ of
  L.Nil -> pure unit
  L.Cons a L.Nil -> void a
  as@(L.Cons _ tail) -> do
    var <- AV.makeVar
    ref <- liftEff $ newRef tail
    let
      run a = do
        attempt a >>= case _ of
          Left err -> AV.putVar var (Just err)
          Right _ ->
            liftEff (readRef ref) >>=
              case _ of
                L.Nil -> AV.putVar var Nothing
                L.Cons _ xs -> liftEff $ writeRef ref xs
    _ <- forkAll (run <$> as)
    AV.peekVar var >>= case _ of
      Nothing -> pure unit
      Just err -> throwError err

handleLifecycle
  :: forall eff t c m
   . (MonadAff (HalogenEffects eff) m, Parallel t m, MonadFork c m)
  => Ref (LifecycleHandlers m)
  -> Eff (HalogenEffects eff)
  ~> m
handleLifecycle lchs f = do
  liftEff $ writeRef lchs { initializers: L.Nil, finalizers: L.Nil }
  result <- liftEff f
  { initializers, finalizers } <- liftEff $ readRef lchs
  fork $ parSequence_ finalizers
  parSequence_ initializers
  pure result

type Renderer h r m eff
  = forall s f z g p i o
   . Ref (LifecycleHandlers m)
  -> Ref (DriverState h r s f z g p i o m)
  -> Eff (HalogenEffects eff) Unit

eval
  :: forall h r eff s'' f z g'' p'' i o a t m
   . (MonadAff (HalogenEffects eff) m, Parallel t m, MonadFork Error m, MonadRec m, MonadError Error m)
  => Renderer h r m eff
  -> Ref (DriverState h r s'' f z g'' p'' i o m)
  -> InputF a (z a)
  -> m a
eval render r =
  case _ of
    RefUpdate (RefLabel p) el next -> do
      liftEff $ modifyRef r \(DriverState st) ->
        DriverState st { refs = SM.alter (const el) p st.refs }
      pure next
    Query q -> evalF r q

  where

  go
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' m)
    -> HalogenF s' z' g' p' o' m
    ~> m
  go ref = case _ of
    GetState k -> do
      DriverState { state } <- liftEff (readRef ref)
      pure (k state)
    ModifyState f -> do
      DriverState (st@{ state, lifecycleHandlers }) <- liftEff (readRef ref)
      case f state of
        Tuple a state' -> do
          liftEff $ writeRef ref (DriverState (st { state = state' }))
          handleLifecycle lifecycleHandlers (render lifecycleHandlers ref)
          pure a
    Subscribe es next -> do
      DriverState ({ subscriptions, fresh }) <- liftEff (readRef ref)
      fork do
        { producer, done } <- ES.unEventSource es
        i <- liftEff do
          i <- modifyRef' fresh (\i -> { state: i + 1, value: i })
          modifyRef subscriptions (map (M.insert i done))
          pure i
        let
          consumer = do
            q <- CR.await
            subs <- lift $ liftEff (readRef subscriptions)
            when (isJust subs) do
              s <- lift $ evalF ref q
              when (s == ES.Listening) consumer
        CR.runProcess (consumer `CR.pullFrom` producer)
        done
        liftEff $ modifyRef subscriptions (map (M.delete i))
      pure next
    Halt msg ->
      throwError (error msg)
    GetSlots k -> do
      DriverState { children } <- liftEff (readRef ref)
      pure $ k $ map unOrdBox $ M.keys children
    CheckSlot p k -> do
      DriverState { component, children } <- liftEff (readRef ref)
      pure $ k $ M.member (component.mkOrdBox p) children
    ChildQuery p cq ->
      evalChildQuery ref p cq
    Raise o a -> do
      DriverState { handler, pendingOuts } <- liftEff (readRef ref)
      queuingHandler handler pendingOuts o
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    -- Fork f ->
    --   FF.unFork (\(FF.ForkF fx k) →
    --     k <<< map ?f <$> fork (evalM ref fx)) f
    GetRef (RefLabel p) k -> do
      DriverState { component, refs } <- liftEff (readRef ref)
      pure $ k $ SM.lookup p refs

  evalChildQuery
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' m)
    -> p'
    -> Coyoneda g'
    ~> m
  evalChildQuery ref p = unCoyoneda \k q -> do
    DriverState st <- liftEff (readRef ref)
    case M.lookup (st.component.mkOrdBox p) st.children of
      Just var -> do
        dsx <- liftEff (readRef var)
        unDriverStateX (\ds -> case ds.prjQuery q of
          Just q' -> k <$> evalF ds.selfRef q'
          Nothing -> throwError (error "Query projection failed for child query")) dsx
      Nothing -> throwError (error "Slot lookup failed for child query")

  evalF
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' m)
    -> z'
    ~> m
  evalF ref q = do
    DriverState st <- liftEff (readRef ref)
    case st.component.eval q of
      HalogenM fx -> runFreeT (go ref) fx

  evalM
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' m)
    -> HalogenM s' z' g' p' o' m
    ~> m
  evalM ref (HalogenM q) = runFreeT (go ref) q

queuingHandler
  :: forall a m eff
   . MonadAff (HalogenEffects eff) m
  => (a -> m Unit)
  -> Ref (Maybe (List (m Unit)))
  -> a
  -> m Unit
queuingHandler handler ref message = do
  queue <- liftEff (readRef ref)
  case queue of
    Nothing ->
      handler message
    Just p ->
      liftEff $ writeRef ref (Just (handler message : p))
