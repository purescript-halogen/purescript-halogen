module Halogen.Query.HalogenF where

import Prelude

import Control.Monad.Free.Trans (hoistFreeT, interpret)

import Data.List as L
import Data.Bifunctor (lmap)

import Halogen.Query.ChildQuery (ChildQuery, hoistChildQuery)
import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.StateF (StateF)

import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

data ParF f x y a = ParF (x -> y -> a) (f x) (f y)
data Par (f :: * -> *) a

mkPar :: forall f x y a. ParF f x y a → Par f a
mkPar = unsafeCoerce

unPar :: forall f x y a r. (ParF f x y a → r) → Par f a → r
unPar = unsafeCoerce

instance functorPar :: Functor (Par f) where
  map f = unPar \(ParF g fx fy) -> mkPar (ParF (\x y -> f (g x y)) fx fy)

hoistPar :: forall f f' a. (f ~> f') -> Par f a -> Par f' a
hoistPar nat = unPar \(ParF f fx fy) -> mkPar (ParF f (nat fx) (nat fy))

--------------------------------------------------------------------------------

-- | The Halogen component algebra
data HalogenF s f g p o m pf a
  = State (StateF s a)
  | Subscribe (EventSource f m) a
  | Lift (m a)
  | Halt String
  | GetSlots (L.List p -> a)
  | ChildQuery (ChildQuery g m p a)
  | Raise o a
  | Par (Par pf a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f g p o m pf) where
  map f = case _ of
    State q -> State (map f q)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots (map f k)
    ChildQuery cq -> ChildQuery (map f cq)
    Raise o a -> Raise o (f a)
    Par p -> Par (f <$> p)

hoistF
  :: forall s f f' g p o m pf
   . Functor m
  => f ~> f'
  -> HalogenF s f g p o m pf
  ~> HalogenF s f' g p o m pf
hoistF nat =
  case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (EventSource (interpret (lmap nat) (runEventSource es))) next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par p

hoistM
  :: forall s f g p o m m' pf
   . Functor m'
  => m ~> m'
  -> HalogenF s f g p o m pf
  ~> HalogenF s f g p o m' pf
hoistM nat =
  case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (EventSource (hoistFreeT nat (runEventSource es))) next
    Lift q -> Lift (nat q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery (hoistChildQuery nat cq)
    Raise o a -> Raise o a
    Par p -> Par p

hoistPF
  :: forall s f g p o m pf pf'
   . pf ~> pf'
  -> HalogenF s f g p o m pf
  ~> HalogenF s f g p o m pf'
hoistPF nat =
  case _ of
    State q -> State q
    Subscribe es next -> Subscribe es next
    Lift q -> Lift q
    Halt msg -> Halt msg
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery cq
    Raise o a -> Raise o a
    Par p -> Par (hoistPar nat p)
