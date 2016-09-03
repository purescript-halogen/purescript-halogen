module Halogen.Query.HalogenM where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Trans (class MonadTrans)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Parallel.Class (class MonadPar)

import Data.Either (either)
import Data.Tuple (Tuple(..))

import Halogen.Query.HalogenF as HF
import Halogen.Query.StateF as SF

newtype HalogenM s f g p o m a = HalogenM (Free (HF.HalogenF s f g p o m (HalogenM s f g p o m)) a)

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
  liftEff eff = HalogenM $ liftF $ HF.Lift $ liftEff eff

instance monadAffHalogenM :: MonadAff eff m ⇒ MonadAff eff (HalogenM s f g p o m) where
  liftAff aff = HalogenM $ liftF $ HF.Lift $ liftAff aff

instance monadParSlamM ∷ MonadPar (HalogenM s f g p o m) where
  par f a b = HalogenM $ liftF $ HF.Par $ HF.mkPar $ HF.ParF f a b

instance monadTransHalogenM :: MonadTrans (HalogenM s f g p o) where
  lift m = HalogenM $ liftF $ HF.Lift m

instance monadRecHalogenM :: MonadRec (HalogenM s f g p o m) where
  tailRecM k a = k a >>= either (tailRecM k) pure

instance monadStateHalogenM :: MonadState s (HalogenM s f g p o m) where
  state f = do
    st <- HalogenM $ liftF $ HF.State $ SF.Get id
    case f st of
      Tuple a st' -> do
        HalogenM $ liftF $ HF.State $ SF.Modify (const st') unit
        pure a

-- TODO: MonadFork

halt :: forall s f g p o m a. String -> HalogenM s f g p o m a
halt msg = HalogenM $ liftF $ HF.Halt msg

hoistF
  :: forall s f f' g m p o
   . Functor m
  => (f ~> f')
  -> HalogenM s f g p o m
  ~> HalogenM s f' g p o m
hoistF nat (HalogenM fa) =
  HalogenM (hoistFree (HF.hoistPF (hoistF nat) <<< HF.hoistF nat) fa)

hoistM
  :: forall s f g m m' p o
   . Functor m'
  => (m ~> m')
  -> HalogenM s f g p o m
  ~> HalogenM s f g p o m'
hoistM nat (HalogenM fa) =
  HalogenM (hoistFree (HF.hoistPF (hoistM nat) <<< HF.hoistM nat) fa)
