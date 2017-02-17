module Halogen.Query.HalogenM where

import Prelude

import Control.Applicative.Free (FreeAp, liftFreeAp, hoistFreeAp)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork, fork)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Free.Trans as FT
import Control.Parallel.Class (class Parallel)

import Data.Bifunctor (lmap)
import Data.Coyoneda (Coyoneda, coyoneda)
import Data.Foreign (Foreign)
import Data.List as L
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple)

import Halogen.Query.EventSource as ES
import Halogen.Query.InputF (RefLabel)

-- | The Halogen component algebra
data HalogenF s (f :: * -> *) g p o m a
  = GetState (s -> a)
  | ModifyState (s -> Tuple a s)
  | Subscribe (ES.EventSource f m) a
  | Halt String
  | GetSlots (L.List p -> a)
  | CheckSlot p (Boolean -> a)
  | ChildQuery p (Coyoneda g a)
  | Raise o a
  | Par (HalogenAp s f g p o m a)
  | GetRef RefLabel (Maybe Foreign -> a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f g p o m) where
  map f = case _ of
    GetState k -> GetState (f <<< k)
    ModifyState k -> ModifyState (lmap f <<< k)
    Subscribe es a -> Subscribe es (f a)
    Halt msg -> Halt msg
    CheckSlot p k -> CheckSlot p (map f k)
    GetSlots k -> GetSlots (map f k)
    ChildQuery p cq -> ChildQuery p (map f cq)
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    GetRef p k -> GetRef p (map f k)

newtype HalogenAp s f g p o m a = HalogenAp (FreeAp (HalogenM s f g p o m) a)

derive instance newtypeHalogenAp :: Newtype (HalogenAp s f g p o m a) _
derive newtype instance functorHalogenAp :: Functor (HalogenAp s f g p o m)
derive newtype instance applyHalogenAp :: Apply (HalogenAp s f g p o m)
derive newtype instance applicativeHalogenAp :: Applicative (HalogenAp s f g p o m)

newtype HalogenM s (f :: * -> *) g p o m a = HalogenM (FT.FreeT (HalogenF s f g p o m) m a)

instance functorHalogenM :: Monad m => Functor (HalogenM s f g p o m) where
  map f (HalogenM fa) = HalogenM (map f fa)

instance applyHalogenM :: Monad m => Apply (HalogenM s f g p o m) where
  apply (HalogenM fa) (HalogenM fb) = HalogenM (apply fa fb)

instance applicativeHalogenM :: Monad m => Applicative (HalogenM s f g p o m) where
  pure a = HalogenM (pure a)

instance bindHalogenM :: Monad m => Bind (HalogenM s f g p o m) where
  bind (HalogenM fa) f = HalogenM (fa >>= \x -> case f x of HalogenM fb -> fb)

instance monadHalogenM :: Monad m => Monad (HalogenM s f g p o m)

instance monadEffHalogenM :: MonadEff eff m => MonadEff eff (HalogenM s f g p o m) where
  liftEff = HalogenM <<< lift <<< liftEff

instance monadAffHalogenM :: MonadAff eff m => MonadAff eff (HalogenM s f g p o m) where
  liftAff = HalogenM <<< lift <<< liftAff

instance parallelHalogenM :: Monad m => Parallel (HalogenAp s f g p o m) (HalogenM s f g p o m) where
  parallel = HalogenAp <<< liftFreeAp
  sequential = HalogenM <<< FT.liftFreeT <<< Par

-- instance monadForkHalogenM :: MonadFork e m => MonadFork e (HalogenM s f g p o m) where
--   fork = HalogenM <<< lift <<< fork

instance monadTransHalogenM :: MonadTrans (HalogenM s f g p o) where
  lift = HalogenM <<< lift

instance monadRecHalogenM :: MonadRec m => MonadRec (HalogenM s f g p o m) where
  tailRecM k a = HalogenM (tailRecM (\x -> case k x of HalogenM y -> y) a)

instance monadStateHalogenM :: Monad m => MonadState s (HalogenM s f g p o m) where
  state = HalogenM <<< FT.liftFreeT <<< ModifyState

instance monadAskHalogenM :: MonadAsk r m => MonadAsk r (HalogenM s f g p o m) where
  ask = HalogenM $ lift $ ask

instance monadTellHalogenM :: MonadTell w m => MonadTell w (HalogenM s f g p o m) where
  tell = HalogenM <<< lift <<< tell

halt :: forall s f g p o m a. Monad m => String -> HalogenM s f g p o m a
halt msg = HalogenM $ FT.liftFreeT $ Halt msg

mkQuery
  :: forall s f g p o m a
   . (Monad m, Eq p)
  => p
  -> g a
  -> HalogenM s f g p o m a
mkQuery p = HalogenM <<< FT.liftFreeT <<< ChildQuery p <<< coyoneda id

getSlots :: forall s f g p o m. Monad m => HalogenM s f g p o m (L.List p)
getSlots = HalogenM $ FT.liftFreeT $ GetSlots id

checkSlot :: forall s f g p o m. Monad m => p -> HalogenM s f g p o m Boolean
checkSlot p = HalogenM $ FT.liftFreeT $ CheckSlot p id

getRef :: forall s f g p o m. Monad m => RefLabel -> HalogenM s f g p o m (Maybe Foreign)
getRef p = HalogenM $ FT.liftFreeT $ GetRef p id

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall s f g p o m. Monad m => ES.EventSource f m -> HalogenM s f g p o m Unit
subscribe es = HalogenM $ FT.liftFreeT $ Subscribe es unit

-- | Raises an output message for the component.
raise :: forall s f g p o m. Monad m => o -> HalogenM s f g p o m Unit
raise o = HalogenM $ FT.liftFreeT $ Raise o unit

hoist
  :: forall s f g p o m m'
   . (Monad m, Functor m')
  => (m ~> m')
  -> HalogenM s f g p o m
  ~> HalogenM s f g p o m'
hoist nat (HalogenM fa) = HalogenM (FT.bimapFreeT go nat fa)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f g p o m'
  go = case _ of
    GetState k -> GetState k
    ModifyState f -> ModifyState f
    Subscribe es next -> Subscribe (ES.hoist nat es) next
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    CheckSlot p k -> CheckSlot p k
    ChildQuery p cq -> ChildQuery p cq
    Raise o a -> Raise o a
    Par p -> Par (over HalogenAp (hoistFreeAp (hoist nat)) p)
    GetRef p k -> GetRef p k
