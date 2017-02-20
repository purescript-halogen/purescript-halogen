module Halogen.Aff.Driver.Eval where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Coroutine as CR
import Control.Monad.Aff (Aff, attempt, forkAff, forkAll)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Eff.Ref (REF, Ref, readRef, modifyRef, modifyRef', writeRef, newRef)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork (fork)
import Control.Monad.Free (foldFree)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parallel, sequential)

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
import Halogen.Query.ForkF as FF
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), HalogenAp(..))
import Halogen.Query.InputF (InputF(..), RefLabel(..))

import Unsafe.Reference (unsafeRefEq)

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
  :: forall eff a
   . Ref (LifecycleHandlers eff)
  -> Eff (HalogenEffects eff) a
  -> Aff (HalogenEffects eff) a
handleLifecycle lchs f = do
  liftEff $ writeRef lchs { initializers: L.Nil, finalizers: L.Nil }
  result <- liftEff f
  { initializers, finalizers } <- liftEff $ readRef lchs
  forkAll finalizers
  parSequenceAff_ initializers
  pure result

type Renderer h r eff
  = forall s f z g p i o
   . Ref (LifecycleHandlers eff)
  -> Ref (DriverState h r s f z g p i o eff)
  -> Eff (HalogenEffects eff) Unit

eval
  :: forall h r eff s'' f z g'' p'' i o a
   . Renderer h r eff
  -> Ref (DriverState h r s'' f z g'' p'' i o eff)
  -> InputF a (z a)
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
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' eff)
    -> HalogenF s' z' g' p' o' (Aff (HalogenEffects eff))
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
      forkAff do
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
    ChildQuery p cq ->
      evalChildQuery ref p cq
    Raise o a -> do
      DriverState { handler, pendingOuts } <- liftEff (readRef ref)
      queuingHandler handler pendingOuts o
      pure a
    Par (HalogenAp p) ->
      sequential $ retractFreeAp $ hoistFreeAp (parallel <<< evalM ref) p
    Fork f ->
      FF.unFork (\(FF.ForkF fx k) →
        k <<< map unsafeCoerceAff <$> fork (evalM ref fx)) f
    GetRef (RefLabel p) k -> do
      DriverState { component, refs } <- liftEff (readRef ref)
      pure $ k $ SM.lookup p refs

  evalChildQuery
    :: forall s' f' z' g' p' i' o'
     . Ref (DriverState h r s' f' z' g' p' i' o' eff)
    -> p'
    -> Coyoneda g'
    ~> Aff (HalogenEffects eff)
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
