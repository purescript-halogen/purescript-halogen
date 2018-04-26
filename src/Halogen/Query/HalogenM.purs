module Halogen.Query.HalogenM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Parallel.Class (class Parallel)
import Data.Bifunctor (lmap)
import Data.Foreign (Foreign)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Halogen.Data.Slot (Slot, SlotStorage)
import Halogen.Data.Slot as Slot
import Halogen.Query.EventSource as ES
import Halogen.Query.ForkF as FF
import Halogen.Query.InputF (RefLabel)
import Unsafe.Coerce (unsafeCoerce)

newtype UnpackQuery ps g o f b =
  UnpackQuery (forall slot m . Applicative m => (slot g o -> m b) -> SlotStorage ps slot -> m (f b))

type QueryBox' ps g o a f b =
  { unpack :: UnpackQuery ps g o f b
  , query :: g b
  , reply :: f b -> a
  }

data QueryBox (ps :: # Type) a

mkQuery' :: forall ps g o a f b. QueryBox' ps g o a f b -> QueryBox ps a
mkQuery' = unsafeCoerce

unQuery :: forall ps a r. (forall g o f b. QueryBox' ps g o a f b -> r) -> QueryBox ps a -> r
unQuery = unsafeCoerce

-- | The Halogen component algebra
data HalogenF s (f :: Type -> Type) (ps :: # Type) o m a
  = State (s -> Tuple a s)
  | Subscribe (ES.EventSource f m) a
  | Lift (m a)
  | Halt String
  | ChildQuery (QueryBox ps a)
  | Raise o a
  | Par (HalogenAp s f ps o m a)
  | Fork (FF.Fork (HalogenM s f ps o m) a)
  | GetRef RefLabel (Maybe Foreign -> a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f ps o m) where
  map f = case _ of
    State k -> State (lmap f <<< k)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt msg -> Halt msg
    ChildQuery cq -> ChildQuery (unQuery (\cq' -> mkQuery' $ cq' { reply = cq'.reply >>> f }) cq)
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork fa -> Fork (map f fa)
    GetRef p k -> GetRef p (map f k)

newtype HalogenAp s f ps o m a = HalogenAp (FreeAp (HalogenM s f ps o m) a)

derive instance newtypeHalogenAp :: Newtype (HalogenAp s f ps o m a) _
derive newtype instance functorHalogenAp :: Functor (HalogenAp s f ps o m)
derive newtype instance applyHalogenAp :: Apply (HalogenAp s f ps o m)
derive newtype instance applicativeHalogenAp :: Applicative (HalogenAp s f ps o m)

newtype HalogenM s (f :: Type -> Type) ps o m a = HalogenM (Free (HalogenF s f ps o m) a)

instance functorHalogenM :: Functor (HalogenM s f ps o m) where
  map f (HalogenM fa) = HalogenM (map f fa)

instance applyHalogenM :: Apply (HalogenM s f ps o m) where
  apply (HalogenM fa) (HalogenM fb) = HalogenM (apply fa fb)

instance applicativeHalogenM :: Applicative (HalogenM s f ps o m) where
  pure a = HalogenM (pure a)

instance bindHalogenM :: Bind (HalogenM s f ps o m) where
  bind (HalogenM fa) f = HalogenM (fa >>= \x -> case f x of HalogenM fb -> fb)

instance monadHalogenM :: Monad (HalogenM s f ps o m)

instance monadEffHalogenM :: MonadEff eff m => MonadEff eff (HalogenM s f ps o m) where
  liftEff eff = HalogenM $ liftF $ Lift $ liftEff eff

instance monadAffHalogenM :: MonadAff eff m => MonadAff eff (HalogenM s f ps o m) where
  liftAff aff = HalogenM $ liftF $ Lift $ liftAff aff

instance parallelHalogenM :: Parallel (HalogenAp s f ps o m) (HalogenM s f ps o m) where
  parallel = HalogenAp <<< liftFreeAp
  sequential = HalogenM <<< liftF <<< Par

instance monadTransHalogenM :: MonadTrans (HalogenM s f ps o) where
  lift m = HalogenM $ liftF $ Lift m

instance monadRecHalogenM :: MonadRec (HalogenM s f ps o m) where
  tailRecM k a = k a >>= go
    where
    go (Loop x) = tailRecM k x
    go (Done y) = pure y

instance monadStateHalogenM :: MonadState s (HalogenM s f ps o m) where
  state = HalogenM <<< liftF <<< State

instance monadAskHalogenM :: MonadAsk r m => MonadAsk r (HalogenM s f ps o m) where
  ask = HalogenM $ liftF $ Lift $ ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (HalogenM s f ps o m) where
  tell = HalogenM <<< liftF <<< Lift <<< tell

halt :: forall s f ps o m a. String -> HalogenM s f ps o m a
halt msg = HalogenM $ liftF $ Halt msg

-- | Sends a query to a child of a component at the specified slot.
query
  :: forall s f o m sym px ps g o' p a
   . RowCons sym (Slot g o' p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> g a
  -> HalogenM s f ps o m (Maybe a)
query sym p query = HalogenM $ liftF $ ChildQuery $ mkQuery'
  { unpack: UnpackQuery \k -> Slot.lookup sym p >>> traverse k
  , query
  , reply: id
  }

-- | Sends a query to all children of a component at a given slot label.
queryAll
  :: forall s f o m sym px ps g o' p a
   . RowCons sym (Slot g o' p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> g a
  -> HalogenM s f ps o m (Map p a)
queryAll sym query = HalogenM $ liftF $ ChildQuery $ mkQuery'
  { unpack: UnpackQuery \k -> Slot.slots sym >>> traverse k
  , query
  , reply: id
  }

getRef :: forall s f ps o m. RefLabel -> HalogenM s f ps o m (Maybe Foreign)
getRef p = HalogenM $ liftF $ GetRef p id

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall s f ps o m. ES.EventSource f m -> HalogenM s f ps o m Unit
subscribe es = HalogenM $ liftF $ Subscribe es unit

-- | Raises an output message for the component.
raise :: forall s f ps o m. o -> HalogenM s f ps o m Unit
raise o = HalogenM $ liftF $ Raise o unit

fork :: forall s f ps o m eff a. MonadAff eff m => HalogenM s f ps o m a -> HalogenM s f ps o m (Error -> m Unit)
fork a = map liftAff <$> HalogenM (liftF $ Fork $ FF.fork a)

imapState
  :: forall s s' f ps o m
   . (s -> s')
  -> (s' -> s)
  -> HalogenM s  f ps o m
  ~> HalogenM s' f ps o m
imapState f f' (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f ps o m ~> HalogenF s' f ps o m
  go = case _ of
    State fs -> State (map f <<< fs <<< f')
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (imapState f f')) p)
    Fork fo -> Fork (FF.hoistFork (imapState f f') fo)
    GetRef p k -> GetRef p k

mapQuery
  :: forall s f f' ps o m
   . Functor m
  => (f ~> f')
  -> HalogenM s f  ps o m
  ~> HalogenM s f' ps o m
mapQuery nat (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f ps o m ~> HalogenF s f' ps o m
  go = case _ of
    State f -> State f
    Subscribe es next -> Subscribe (ES.interpret nat es) next
    Lift q -> Lift q
    Halt msg -> Halt msg
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapQuery nat)) p)
    Fork f -> Fork (FF.hoistFork (mapQuery nat) f)
    GetRef p k -> GetRef p k

mapOutput
  :: forall s f ps o o' m
   . (o -> o')
  -> HalogenM s f ps o  m
  ~> HalogenM s f ps o' m
mapOutput f (HalogenM h) = HalogenM (hoistFree go h)
  where
  go :: HalogenF s f ps o m ~> HalogenF s f ps o' m
  go = case _ of
    State fs -> State fs
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise (f o) a
    Par p -> Par (over HalogenAp (hoistFreeAp (mapOutput f)) p)
    Fork fo -> Fork (FF.hoistFork (mapOutput f) fo)
    GetRef p k -> GetRef p k

hoist
  :: forall s f ps o m m'
   . Functor m'
  => (m ~> m')
  -> HalogenM s f ps o m
  ~> HalogenM s f ps o m'
hoist nat (HalogenM fa) = HalogenM (hoistFree go fa)
  where
  go :: HalogenF s f ps o m ~> HalogenF s f ps o m'
  go = case _ of
    State f -> State f
    Subscribe es next -> Subscribe (ES.hoist nat es) next
    Lift q -> Lift (nat q)
    Halt msg -> Halt msg
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (hoist nat)) p)
    Fork f -> Fork (FF.hoistFork (hoist nat) f)
    GetRef p k -> GetRef p k
