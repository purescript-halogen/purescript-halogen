module Halogen.Query.HalogenF where

import Prelude

import Control.Applicative.Free (FreeAp, liftAp, hoistAp, lowerAp)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Fork (class MonadFork)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Free.Trans (bimapFreeT, interpret)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Parallel.Class (class Parallel)

import Data.Bifunctor (lmap)
import Data.List as L
import Data.Newtype (class Newtype, over)
import Data.Tuple (Tuple(..))

import Halogen.Query.ChildQuery as CQ
import Halogen.Query.EventSource as ES
import Halogen.Query.StateF as SF
import Halogen.Query.ForkF as FF

-- | The Halogen component algebra
data HalogenF s (f :: * -> *) g p o m a
  = State (SF.StateF s a)
  | Subscribe (ES.EventSource (HalogenM s f g p o m) m) a
  | Lift (m a)
  | Halt String
  | GetSlots (L.List p -> a)
  | ChildQuery (CQ.ChildQuery g m p a)
  | Raise o a
  | Par (HalogenAp s f g p o m a)
  | Fork (FF.Fork (HalogenM s f g p o m) a)

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

newtype HalogenAp s f g p o m a = HalogenAp (FreeAp (HalogenM s f g p o m) a)

derive instance newtypeHalogenAp :: Newtype (HalogenAp s f g p o m a) _
derive newtype instance functorHalogenAp :: Functor (HalogenAp s f g p o m)
derive newtype instance applyHalogenAp :: Apply (HalogenAp s f g p o m)
derive newtype instance applicativeHalogenAp :: Applicative (HalogenAp s f g p o m)

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

instance parallelHalogenM ∷ Parallel (HalogenAp s f g p o m) (HalogenM s f g p o m) where
  parallel = HalogenAp <<< liftAp
  sequential = HalogenM <<< liftF <<< Par

instance monadForkHalogenM ∷ MonadAff eff m ⇒ MonadFork Error (HalogenM s f g p o m) where
  fork a =
    map (liftAff :: Aff eff ~> HalogenM s f g p o m)
      <$> HalogenM (liftF $ Fork $ FF.fork a)

instance monadTransHalogenM :: MonadTrans (HalogenM s f g p o) where
  lift m = HalogenM $ liftF $ Lift m

instance monadRecHalogenM :: MonadRec (HalogenM s f g p o m) where
  tailRecM k a = k a >>= go
    where
    go (Loop a) = tailRecM k a
    go (Done b) = pure b

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
    Par p -> Par (over HalogenAp (hoistAp (hoistF nat)) p)
    Fork f -> Fork (FF.hoistFork (hoistF nat) f)

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
    Par p -> Par (over HalogenAp (hoistAp (hoistM nat)) p)
    Fork f -> Fork (FF.hoistFork (hoistM nat) f)
