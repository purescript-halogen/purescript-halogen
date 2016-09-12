module Halogen.Query.HalogenF where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Free.Trans (bimapFreeT, interpret)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans (class MonadTrans)
import Control.Monad.Fork (class MonadFork, Canceler, hoistCanceler)
import Control.Parallel.Class (class MonadPar)

import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.List as L
import Data.Tuple (Tuple(..))

import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Halogen.Query.StateF as SF
import Halogen.Query.ParF as PF
import Halogen.Query.ForkF as FF

newtype HalogenM s (f :: * -> *) g p o m a = HalogenM (Free (HalogenF s f g p o m) a)

instance functorHalogenM :: Functor (HalogenM s f g p o m) where
  map f (HalogenM fa) = HalogenM (map f fa)

instance applyHalogenM :: Apply (HalogenM s f g p o m) where
  apply (HalogenM fa) (HalogenM fb) = HalogenM (apply fa fb)

instance applicativeHalogenM :: Applicative (HalogenM s f g p o m) where
  pure a = HalogenM (pure a)

instance bindHalogenM :: Bind (HalogenM s f g p o m) where
  bind (HalogenM fa) f = HalogenM (fa >>= \x -> case f x of HalogenM fb -> fb)

instance monadHalogenM :: Monad (HalogenM s f g p o m)

instance monadEffHalogenM :: MonadEff eff m ⇒ MonadEff eff (HalogenM s f g p o m) where
  liftEff eff = HalogenM $ liftF $ Lift $ liftEff eff

instance monadAffHalogenM :: MonadAff eff m ⇒ MonadAff eff (HalogenM s f g p o m) where
  liftAff aff = HalogenM $ liftF $ Lift $ liftAff aff

instance monadParSlamM ∷ MonadPar (HalogenM s f g p o m) where
  par f a b = HalogenM $ liftF $ Par $ PF.mkPar $ PF.ParF f a b

instance monadForkHalogenM ∷ MonadFork (HalogenM s f g p o m) where
  fork a = HalogenM $ liftF $ Fork $ FF.mkFork $ FF.ForkF a id
  cancelWith a c = HalogenM $ liftF $ Cancel a c

instance monadTransHalogenM :: MonadTrans (HalogenM s f g p o) where
  lift m = HalogenM $ liftF $ Lift m

instance monadRecHalogenM :: MonadRec (HalogenM s f g p o m) where
  tailRecM k a = k a >>= either (tailRecM k) pure

instance monadStateHalogenM :: MonadState s (HalogenM s f g p o m) where
  state f = do
    st <- HalogenM $ liftF $ State $ SF.Get id
    case f st of
      Tuple a st' -> do
        HalogenM $ liftF $ State $ SF.Modify (const st') unit
        pure a

halt :: forall s f g p o m a. String -> HalogenM s f g p o m a
halt msg = HalogenM $ liftF $ Halt msg

hoistF
  :: forall s f f' g p o m
   . Functor m
  => (f ~> f')
  -> HalogenM s f g p o m
  ~> HalogenM s f' g p o m
hoistF nat (HalogenM fa) = HalogenM (hoistFree go fa)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f' g p o m
  go = case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (ES.EventSource (interpret (lmap (hoistF nat)) (ES.runEventSource es))) next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (PF.hoistPar (hoistF nat) p)
    Fork f -> Fork (FF.hoistFork (hoistF nat) f)
    Cancel m c -> Cancel (hoistF nat m) (hoistCanceler (hoistF nat) c)

hoistM
  :: forall s f g p o m m'
   . Functor m'
  => (m ~> m')
  -> HalogenM s f g p o m
  ~> HalogenM s f g p o m'
hoistM nat (HalogenM fa) = HalogenM (hoistFree go fa)
  where
  go :: HalogenF s f g p o m ~> HalogenF s f g p o m'
  go = case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (ES.EventSource (bimapFreeT (lmap (hoistM nat)) nat (ES.runEventSource es))) next
    Lift q -> Lift (nat q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery (CQ.hoistChildQuery nat cq)
    Raise o a -> Raise o a
    Par p -> Par (PF.hoistPar (hoistM nat) p)
    Fork f -> Fork (FF.hoistFork (hoistM nat) f)
    Cancel m c -> Cancel (hoistM nat m) (hoistCanceler (hoistM nat) c)


-- | The Halogen component algebra
data HalogenF s (f :: * -> *) g p o m a
  = State (SF.StateF s a)
  | Subscribe (ES.EventSource (HalogenM s f g p o m) m) a
  | Lift (m a)
  | Halt String
  | GetSlots (L.List p -> a)
  | ChildQuery (CQ.ChildQuery g m p a)
  | Raise o a
  | Par (PF.Par (HalogenM s f g p o m) a)
  | Fork (FF.Fork (HalogenM s f g p o m) a)
  | Cancel (HalogenM s f g p o m a) (Canceler (HalogenM s f g p o m))

instance functorHalogenF :: Functor m => Functor (HalogenF s f g p o m) where
  map f = case _ of
    State q -> State (map f q)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots (map f k)
    ChildQuery cq -> ChildQuery (map f cq)
    Raise o a -> Raise o (f a)
    Par pa -> Par (map f pa)
    Fork fa -> Fork (map f fa)
    Cancel ma c -> Cancel (map f ma) c
