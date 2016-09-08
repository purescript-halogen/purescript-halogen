module Halogen.Query.HalogenF where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Free.Trans (hoistFreeT, interpret)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans (class MonadTrans)
import Control.Parallel.Class (class MonadPar)

import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.List as L
import Data.Tuple (Tuple(..))

import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Halogen.Query.StateF as SF
import Halogen.Query.ParF as PF

newtype HalogenM s f g p o m a = HalogenM (Free (HalogenF s f g p o m) a)

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

-- TODO: MonadFork

halt :: forall s f g p o m a. String -> HalogenM s f g p o m a
halt msg = HalogenM $ liftF $ Halt msg

hoistF
  :: forall s f f' g m p o
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
      Subscribe (ES.EventSource (interpret (lmap nat) (ES.runEventSource es))) next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (PF.hoistPar (hoistF nat) p)

hoistM
  :: forall s f g m m' p o
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
        Subscribe (ES.EventSource (hoistFreeT nat (ES.runEventSource es))) next
      Lift q -> Lift (nat q)
      Halt msg -> Halt msg
      GetSlots k -> GetSlots k
      ChildQuery cq -> ChildQuery (CQ.hoistChildQuery nat cq)
      Raise o a -> Raise o a
      Par p -> Par (PF.hoistPar (hoistM nat) p)


--------------------------------------------------------------------------------

-- | The Halogen component algebra
data HalogenF s f g p o m a
  = State (SF.StateF s a)
  | Subscribe (ES.EventSource f m) a
  | Lift (m a)
  | Halt String
  | GetSlots (L.List p -> a)
  | ChildQuery (CQ.ChildQuery g m p a)
  | Raise o a
  | Par (PF.Par (HalogenM s f g p o m) a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f g p o m) where
  map f = case _ of
    State q -> State (map f q)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots (map f k)
    ChildQuery cq -> ChildQuery (map f cq)
    Raise o a -> Raise o (f a)
    Par p -> Par (f <$> p)
