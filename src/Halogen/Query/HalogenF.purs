module Halogen.Query.HalogenF
  ( HalogenF(..)
  , hoistHalogenF
  , hoistHalogenM
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Free.Trans (hoistFreeT, interpret)
import Control.Plus (class Plus)

import Data.List as L
import Data.Bifunctor (lmap)
import Data.Profunctor as PF
import Data.Maybe (Maybe)
import Data.Coyoneda (liftCoyoneda)
import Data.Identity (Identity)

import Halogen.Query.ChildQuery (ChildQuery, ChildQueryF(..), mkChildQuery)
import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.StateF (StateF)

import Unsafe.Coerce (unsafeCoerce)

-- | The Halogen component algebra
data HalogenF s f g m p a
  = State (StateF s a)
  | Subscribe (EventSource f m) a
  | Lift (m a)
  | Halt
  | GetSlots (L.List p -> a)
  | RunQuery p (ChildQuery g a) (Maybe (Identity ~> Identity) -> a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f g m p) where
  map f = case _ of
    State q -> State (map f q)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt -> Halt
    GetSlots k -> GetSlots (map f k)
    RunQuery cq k -> RunQuery (map f cq) (PF.dimap ?l ?r k)

instance affableHalogenF :: Affable eff m => Affable eff (HalogenF s f g m p) where
  fromAff = Lift <<< fromAff

instance altHalogenF :: Functor m => Alt (HalogenF s f g m p) where
  alt Halt h = h
  alt h _ = h

instance plusHalogenF :: Functor m => Plus (HalogenF s f g m p) where
  empty = Halt

query
  :: forall s f g m p a
   . Applicative m
  => g a
  -> p
  -> Free (HalogenF s f g m p) (Maybe a)
query q p =
  liftF $ RunQuery p (mkChildQuery $ ChildQueryF $ liftCoyoneda q) \k -> ?who k

hoistHalogenF
  :: forall s f f' g m p
   . Functor m
  => f ~> f'
  -> HalogenF s f g m p
  ~> HalogenF s f' g m p
hoistHalogenF eta =
  case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (EventSource (interpret (lmap eta) (runEventSource es))) next
    Lift q -> Lift q
    Halt -> Halt
    GetSlots k -> GetSlots k
    RunQuery p k -> RunQuery p k

hoistHalogenM
  :: forall s f g m m' p
   . Functor m'
  => m ~> m'
  -> HalogenF s f g m p
  ~> HalogenF s f g m' p
hoistHalogenM eta =
  case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (EventSource (hoistFreeT eta (runEventSource es))) next
    Lift q -> Lift (eta q)
    Halt -> Halt
    GetSlots k -> GetSlots k
    RunQuery p k -> RunQuery p k
