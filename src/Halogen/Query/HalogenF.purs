module Halogen.Query.HalogenF
  ( HalogenF(..)
  , hoistHalogenF
  , hoistHalogenM
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Free.Trans (hoistFreeT, interpret)
import Control.Plus (class Plus)

import Data.List as L
import Data.Bifunctor (lmap)

import Halogen.Query.ChildQuery (ChildQuery, hoistChildQuery)
import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.StateF (StateF)

-- | The Halogen component algebra
data HalogenF s f g m p a
  = State (StateF s a)
  | Subscribe (EventSource f m) a
  | Lift (m a)
  | Halt
  | GetSlots (L.List p -> a)
  | ChildQuery (ChildQuery g m p a)

instance functorHalogenF :: Functor m => Functor (HalogenF s f g m p) where
  map f = case _ of
    State q -> State (map f q)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt -> Halt
    GetSlots k -> GetSlots (map f k)
    ChildQuery cq -> ChildQuery (map f cq)

instance affableHalogenF :: Affable eff m => Affable eff (HalogenF s f g m p) where
  fromAff = Lift <<< fromAff

instance altHalogenF :: Functor m => Alt (HalogenF s f g m p) where
  alt Halt h = h
  alt h _ = h

instance plusHalogenF :: Functor m => Plus (HalogenF s f g m p) where
  empty = Halt

hoistHalogenF
  :: forall s f f' g m p
   . Functor m
  => f ~> f'
  -> HalogenF s f g m p
  ~> HalogenF s f' g m p
hoistHalogenF nat =
  case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (EventSource (interpret (lmap nat) (runEventSource es))) next
    Lift q -> Lift q
    Halt -> Halt
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery cq

hoistHalogenM
  :: forall s f g m m' p
   . Functor m'
  => m ~> m'
  -> HalogenF s f g m p
  ~> HalogenF s f g m' p
hoistHalogenM nat =
  case _ of
    State q -> State q
    Subscribe es next ->
      Subscribe (EventSource (hoistFreeT nat (runEventSource es))) next
    Lift q -> Lift (nat q)
    Halt -> Halt
    GetSlots k -> GetSlots k
    ChildQuery cq -> ChildQuery (hoistChildQuery nat cq)
