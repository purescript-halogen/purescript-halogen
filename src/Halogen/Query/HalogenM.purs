module Halogen.Query.HalogenM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (class Parallel)
import Data.Bifunctor (lmap)
import Data.Coyoneda (Coyoneda, coyoneda, hoistCoyoneda)
import Data.List as L
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Halogen.Query.EventSource as ES
import Halogen.Query.ForkF as FF
import Halogen.Query.InputF (RefLabel)
import Web.DOM (Element)

-- | The Halogen component algebra
data HalogenF s (f :: Type -> Type) g p o m a
  = State (s -> Tuple a s)
  | Subscribe (ES.EventSource f m) a
  | Lift (m a)
  | Halt String
  | GetSlots (L.List p -> a)
  | CheckSlot p (Boolean -> a)
  | ChildQuery p (Coyoneda g a)
  | Raise o a
  | Par (HalogenAp s f g p o m a)
  | Fork (FF.Fork (HalogenM s f g p o m) a)
  | GetRef RefLabel (Maybe Element -> a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f g p o m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt msg -> Halt msg
    CheckSlot p k -> CheckSlot p (map f k)
    GetSlots k -> GetSlots (map f k)
    ChildQuery p cq -> ChildQuery p (map f cq)
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork fa -> Fork (map f fa)
    GetRef p k -> GetRef p (map f k)

newtype HalogenAp s f g p o m a = HalogenAp (FreeAp (HalogenM s f g p o m) a)

derive instance newtypeHalogenAp :: Newtype (HalogenAp s f g p o m a) _
derive newtype instance functorHalogenAp :: Functor (HalogenAp s f g p o m)
derive newtype instance applyHalogenAp :: Apply (HalogenAp s f g p o m)
derive newtype instance applicativeHalogenAp :: Applicative (HalogenAp s f g p o m)

newtype HalogenM s (f :: Type -> Type) g p o m a = HalogenM (Free (HalogenF s f g p o m) a)

instance functorHalogenM :: Functor (HalogenM s f g p o m) where
  map f (HalogenM fa) = HalogenM (map f fa)

instance applyHalogenM :: Apply (HalogenM s f g p o m) where
  apply (HalogenM fa) (HalogenM fb) = HalogenM (apply fa fb)

instance applicativeHalogenM :: Applicative (HalogenM s f g p o m) where
  pure a = HalogenM (pure a)

instance bindHalogenM :: Bind (HalogenM s f g p o m) where
  bind (HalogenM fa) f = HalogenM (fa >>= \x -> case f x of HalogenM fb -> fb)

instance monadHalogenM :: Monad (HalogenM s f g p o m)

instance monadEffectHalogenM :: MonadEffect m => MonadEffect (HalogenM s f g p o m) where
  liftEffect eff = HalogenM $ liftF $ Lift $ liftEffect eff

instance monadAffHalogenM :: MonadAff m => MonadAff (HalogenM s f g p o m) where
  liftAff aff = HalogenM $ liftF $ Lift $ liftAff aff

instance parallelHalogenM :: Parallel (HalogenAp s f g p o m) (HalogenM s f g p o m) where
  parallel = HalogenAp <<< liftFreeAp
  sequential = HalogenM <<< liftF <<< Par

instance monadTransHalogenM :: MonadTrans (HalogenM s f g p o) where
  lift m = HalogenM $ liftF $ Lift m

instance monadRecHalogenM :: MonadRec (HalogenM s f g p o m) where
  tailRecM k a = k a >>= go
    where
    go (Loop x) = tailRecM k x
    go (Done y) = pure y

instance monadStateHalogenM :: MonadState s (HalogenM s f g p o m) where
  state = HalogenM <<< liftF <<< State

instance monadAskHalogenM :: MonadAsk r m => MonadAsk r (HalogenM s f g p o m) where
  ask = HalogenM $ liftF $ Lift $ ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (HalogenM s f g p o m) where
  tell = HalogenM <<< liftF <<< Lift <<< tell

halt :: forall s f g p o m a. String -> HalogenM s f g p o m a
halt msg = HalogenM $ liftF $ Halt msg

mkQuery
  :: forall s f g p o m a
   . Eq p
  => p
  -> g a
  -> HalogenM s f g p o m a
mkQuery p = HalogenM <<< liftF <<< ChildQuery p <<< coyoneda identity

getSlots :: forall s f g p o m. HalogenM s f g p o m (L.List p)
getSlots = HalogenM $ liftF $ GetSlots identity

checkSlot :: forall s f g p o m. p -> HalogenM s f g p o m Boolean
checkSlot p = HalogenM $ liftF $ CheckSlot p identity

getRef :: forall s f g p o m. RefLabel -> HalogenM s f g p o m (Maybe Element)
getRef p = HalogenM $ liftF $ GetRef p identity

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall s f g p o m. ES.EventSource f m -> HalogenM s f g p o m Unit
subscribe es = HalogenM $ liftF $ Subscribe es unit

-- | Raises an output message for the component.
raise :: forall s f g p o m. o -> HalogenM s f g p o m Unit
raise o = HalogenM $ liftF $ Raise o unit

fork :: forall s f g p o m a. MonadAff m => HalogenM s f g p o m a -> HalogenM s f g p o m (Error -> m Unit)
fork a = map liftAff <$> HalogenM (liftF $ Fork $ FF.fork a)

imapState
  :: forall s s' f g p o m
   . (s -> s')
  -> (s' -> s)
  -> HalogenM s  f g p o m
  ~> HalogenM s' f g p o m
imapState f f' (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f g p o m ~> HalogenF s' f g p o m
  go = case _ of
    State fs -> State (map f <<< fs <<< f')
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    CheckSlot p k -> CheckSlot p k
    ChildQuery p cq -> ChildQuery p cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (imapState f f')) p)
    Fork fo -> Fork (FF.hoistFork (imapState f f') fo)
    GetRef p k -> GetRef p k

mapQuery
  :: forall s f f' g p o m
   . Functor m
  => (f ~> f')
  -> HalogenM s f  g p o m
  ~> HalogenM s f' g p o m
mapQuery nat (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f' g p o m
  go = case _ of
    State f -> State f
    Subscribe es next -> Subscribe (ES.interpret nat es) next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    CheckSlot p k -> CheckSlot p k
    ChildQuery p cq -> ChildQuery p cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapQuery nat)) p)
    Fork f -> Fork (FF.hoistFork (mapQuery nat) f)
    GetRef p k -> GetRef p k

mapChildQuery
  :: forall s f g g' p o m
   . (g ~> g')
  -> HalogenM s f g  p o m
  ~> HalogenM s f g' p o m
mapChildQuery nat (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f g' p o m
  go = case _ of
    State f -> State f
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    CheckSlot p k -> CheckSlot p k
    ChildQuery p cq -> ChildQuery p (hoistCoyoneda nat cq)
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapChildQuery nat)) p)
    Fork f -> Fork (FF.hoistFork (mapChildQuery nat) f)
    GetRef p k -> GetRef p k

imapSlots
  :: forall s f g p p' o m
   . (p -> p')
  -> (p' -> p)
  -> HalogenM s f g p  o m
  ~> HalogenM s f g p' o m
imapSlots f f' (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f g p' o m
  go = case _ of
    State fs -> State fs
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots (k <<< map f')
    CheckSlot p k -> CheckSlot (f p) k
    ChildQuery p cq -> ChildQuery (f p) cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (imapSlots f f')) p)
    Fork fo -> Fork (FF.hoistFork (imapSlots f f') fo)
    GetRef p k -> GetRef p k

mapOutput
  :: forall s f g p o o' m
   . (o -> o')
  -> HalogenM s f g p o  m
  ~> HalogenM s f g p o' m
mapOutput f (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f g p o' m
  go = case _ of
    State fs -> State fs
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    CheckSlot p k -> CheckSlot p k
    ChildQuery p cq -> ChildQuery p cq
    Raise o a -> Raise (f o) a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapOutput f)) p)
    Fork fo -> Fork (FF.hoistFork (mapOutput f) fo)
    GetRef p k -> GetRef p k

hoist
  :: forall s f g p o m m'
   . Functor m'
  => (m ~> m')
  -> HalogenM s f g p o m
  ~> HalogenM s f g p o m'
hoist nat (HalogenM fa) = HalogenM (hoistFree go fa)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f g p o m'
  go = case _ of
    State f -> State f
    Subscribe es next -> Subscribe (ES.hoist nat es) next
    Lift q -> Lift (nat q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    CheckSlot p k -> CheckSlot p k
    ChildQuery p cq -> ChildQuery p cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (hoist nat)) p)
    Fork f -> Fork (FF.hoistFork (hoist nat) f)
    GetRef p k -> GetRef p k
