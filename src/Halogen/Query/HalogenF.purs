module Halogen.Query.HalogenF
  ( HalogenF(..)
  , hoistF
  , hoistM
  ) where

import Prelude

import Control.Monad.Free.Trans (hoistFreeT, interpret)

import Data.List as L
import Data.Bifunctor (lmap)

import Halogen.Query.ChildQuery (ChildQuery, hoistChildQuery)
import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.StateF (StateF)

-- | The Halogen component algebra
data HalogenF s f g m p o a
  = State (StateF s a)
  | Subscribe (EventSource f m) a
  | Lift (m a)
  | Halt String
  | GetSlots (L.List p -> a)
  | ChildQuery (ChildQuery g m p a)
  | Raise o a

instance functorHalogenF :: Functor m => Functor (HalogenF s f g m p o) where
  map f = case _ of
    State q -> State (map f q)
    Subscribe es a -> Subscribe es (f a)
    Lift q -> Lift (map f q)
    Halt msg -> Halt msg
    GetSlots k -> GetSlots (map f k)
    ChildQuery cq -> ChildQuery (map f cq)
    Raise o a -> Raise o (f a)

hoistF
  :: forall s f f' g m p o
   . Functor m
  => f ~> f'
  -> HalogenF s f g m p o
  ~> HalogenF s f' g m p o
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

hoistM
  :: forall s f g m m' p o
   . Functor m'
  => m ~> m'
  -> HalogenF s f g m p o
  ~> HalogenF s f g m' p o
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
