module Halogen.Query.HalogenM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (class Parallel)
import Data.Bifunctor (lmap)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Data.Slot (Slot)
import Halogen.Data.Slot as Slot
import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Halogen.Query.Input (RefLabel)
import Prim.Row as Row
import Web.DOM (Element)

-- | The Halogen component eval algebra.
data HalogenF s act ps o m a
  = State (s -> Tuple a s)
  | Subscribe (SubscriptionId -> ES.EventSource m act) (SubscriptionId -> a)
  | Unsubscribe SubscriptionId a
  | Lift (m a)
  | ChildQuery (CQ.ChildQueryBox ps a)
  | Raise o a
  | Par (HalogenAp s act ps o m a)
  | Fork (HalogenM' s act ps o m Unit) (ForkId -> a)
  | Kill ForkId a
  | GetRef RefLabel (Maybe Element -> a)

instance functorHalogenF :: Functor m => Functor (HalogenF s act ps o m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Subscribe fes k -> Subscribe fes (f <<< k)
    Unsubscribe sid a -> Unsubscribe sid (f a)
    Lift q -> Lift (map f q)
    ChildQuery cq -> ChildQuery (map f cq)
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork hmu k -> Fork hmu (f <<< k)
    Kill fid a -> Kill fid (f a)
    GetRef p k -> GetRef p (f <<< k)

-- | The Halogen component eval effect monad.
newtype HalogenM' s act ps o m a = HalogenM (Free (HalogenF s act ps o m) a)

-- | The Halogen component eval effect monad, for components constructed with
-- | the `component` smart constructor. This is more constrained than
-- | `HalogenM'` as it only allows for query algebra actions (kind
-- | `Type -> Type` rather than `Type`).
type HalogenM s act = HalogenM' s (act Unit)

derive newtype instance functorHalogenM :: Functor (HalogenM' s act ps o m)
derive newtype instance applyHalogenM :: Apply (HalogenM' s act ps o m)
derive newtype instance applicativeHalogenM :: Applicative (HalogenM' s act ps o m)
derive newtype instance bindHalogenM :: Bind (HalogenM' s act ps o m)
derive newtype instance monadHalogenM :: Monad (HalogenM' s act ps o m)

instance monadEffectHalogenM :: MonadEffect m => MonadEffect (HalogenM' s act ps o m) where
  liftEffect = HalogenM <<< liftF <<< Lift <<< liftEffect

instance monadAffHalogenM :: MonadAff m => MonadAff (HalogenM' s act ps o m) where
  liftAff = HalogenM <<< liftF <<< Lift <<< liftAff

instance parallelHalogenM :: Parallel (HalogenAp s act ps o m) (HalogenM' s act ps o m) where
  parallel = HalogenAp <<< liftFreeAp
  sequential = HalogenM <<< liftF <<< Par

instance monadTransHalogenM :: MonadTrans (HalogenM' s act ps o) where
  lift = HalogenM <<< liftF <<< Lift

instance monadRecHalogenM :: MonadRec (HalogenM' s act ps o m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y

instance monadStateHalogenM :: MonadState s (HalogenM' s act ps o m) where
  state = HalogenM <<< liftF <<< State

instance monadAskHalogenM :: MonadAsk r m => MonadAsk r (HalogenM' s act ps o m) where
  ask = HalogenM $ liftF $ Lift ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (HalogenM' s act ps o m) where
  tell = HalogenM <<< liftF <<< Lift <<< tell

instance monadThrowHalogenM :: MonadThrow e m => MonadThrow e (HalogenM' s act ps o m) where
  throwError = HalogenM <<< liftF <<< Lift <<< throwError

-- | An applicative-only version of `HalogenM` to allow for parallel evaluation.
newtype HalogenAp s act ps o m a = HalogenAp (FreeAp (HalogenM' s act ps o m) a)

derive instance newtypeHalogenAp :: Newtype (HalogenAp s f ps o m a) _
derive newtype instance functorHalogenAp :: Functor (HalogenAp s f ps o m)
derive newtype instance applyHalogenAp :: Apply (HalogenAp s f ps o m)
derive newtype instance applicativeHalogenAp :: Applicative (HalogenAp s f ps o m)

-- | The ID value associated with a subscription. Allows the subscription to be
-- | stopped at a later time.
newtype SubscriptionId = SubscriptionId Int

derive newtype instance eqSubscriptionId :: Eq SubscriptionId
derive newtype instance ordSubscriptionId :: Ord SubscriptionId

-- | Subscribes a component to an `EventSource`.
subscribe :: forall s act ps o m. ES.EventSource m act -> HalogenM' s act ps o m SubscriptionId
subscribe es = HalogenM $ liftF $ Subscribe (\_ -> es) identity

-- | An alternative to `subscribe`, intended for subscriptions that unsubscribe
-- | themselves. Instead of returning the `SubscriptionId` from `subscribe'`, it
-- | is passed into an `EventSource` constructor. This allows emitted queries
-- | to include the `SubscriptionId`, rather than storing it in the state of the
-- | component.
subscribe' :: forall s act ps o m. (SubscriptionId -> ES.EventSource m act) -> HalogenM' s act ps o m Unit
subscribe' esc = HalogenM $ liftF $ Subscribe esc (const unit)

-- | Unsubscribes a component from an `EventSource`. If the subscription
-- | associated with the ID has already ended this will have no effect.
unsubscribe :: forall s act ps o m. SubscriptionId -> HalogenM' s act ps o m Unit
unsubscribe sid = HalogenM $ liftF $ Unsubscribe sid unit

-- | Sends a query to a child of a component at the specified slot.
query
  :: forall s act o m sym px ps f o' p a
   . Row.Cons sym (Slot f o' p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> f a
  -> HalogenM' s act ps o m (Maybe a)
query sym p q = HalogenM $ liftF $ ChildQuery $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k -> traverse k <<< Slot.lookup sym p) q identity

-- | Sends a query to all children of a component at a given slot label.
queryAll
  :: forall s act o m sym px ps f o' p a
   . Row.Cons sym (Slot f o' p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> f a
  -> HalogenM' s act ps o m (Map p a)
queryAll sym q = HalogenM $ liftF $ ChildQuery $ CQ.mkChildQueryBox $
  CQ.ChildQuery (\k -> traverse k <<< Slot.slots sym) q identity

getRef :: forall s act ps o m. RefLabel -> HalogenM' s act ps o m (Maybe Element)
getRef p = HalogenM $ liftF $ GetRef p identity

-- | Raises an output message for the component.
raise :: forall s act ps o m. o -> HalogenM' s act ps o m Unit
raise o = HalogenM $ liftF $ Raise o unit

newtype ForkId = ForkId Int

derive newtype instance eqForkId :: Eq ForkId
derive newtype instance ordForkId :: Ord ForkId

fork :: forall s act ps o m. MonadAff m => HalogenM' s act ps o m Unit -> HalogenM' s act ps o m ForkId
fork hmu = HalogenM $ liftF $ Fork hmu identity

kill :: forall s act ps o m. MonadAff m => ForkId -> HalogenM' s act ps o m Unit
kill fid = HalogenM $ liftF $ Kill fid unit

imapState
  :: forall s s' act ps o m
   . (s -> s')
  -> (s' -> s)
  -> HalogenM' s act ps o m
  ~> HalogenM' s' act ps o m
imapState f f' (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s act ps o m ~> HalogenF s' act ps o m
  go = case _ of
    State fs -> State (map f <<< fs <<< f')
    Subscribe fes k -> Subscribe fes k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (imapState f f')) p)
    Fork hmu k -> Fork (imapState f f' hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k

mapAction
  :: forall s act act' ps o m
   . Functor m
  => (act -> act')
  -> HalogenM' s act ps o m
  ~> HalogenM' s act' ps o m
mapAction f (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s act ps o m ~> HalogenF s act' ps o m
  go = case _ of
    State fs -> State fs
    Subscribe fes k -> Subscribe (map f <<< fes) k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapAction f)) p)
    Fork hmu k -> Fork (mapAction f hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k

mapOutput
  :: forall s act ps o o' m
   . (o -> o')
  -> HalogenM' s act ps o m
  ~> HalogenM' s act ps o' m
mapOutput f (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s act ps o m ~> HalogenF s act ps o' m
  go = case _ of
    State fs -> State fs
    Subscribe fes k -> Subscribe fes k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift q
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise (f o) a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapOutput f)) p)
    Fork hmu k -> Fork (mapOutput f hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k

hoist
  :: forall s act ps o m m'
   . Functor m'
  => (m ~> m')
  -> HalogenM' s act ps o m
  ~> HalogenM' s act ps o m'
hoist nat (HalogenM fa) = HalogenM (hoistFree go fa)
  where
  go :: HalogenF s act ps o m ~> HalogenF s act ps o m'
  go = case _ of
    State f -> State f
    Subscribe fes k -> Subscribe (ES.hoist nat <<< fes) k
    Unsubscribe sid a -> Unsubscribe sid a
    Lift q -> Lift (nat q)
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (hoist nat)) p)
    Fork hmu k -> Fork (hoist nat hmu) k
    Kill fid a -> Kill fid a
    GetRef p k -> GetRef p k
