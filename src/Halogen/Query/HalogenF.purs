module Halogen.Query.HalogenF
  ( HalogenF(..)
  -- , hoistHalogenF
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Plus (class Plus)

import Data.List as L
import Data.Maybe (Maybe)
import Data.Profunctor as PF

import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.StateF (StateF)

-- | The Halogen component algebra
data HalogenF s f m p a
  = StateHF (StateF s a)
  | SubscribeHF (EventSource f m) a
  | QueryFHF (f a)
  | QueryGHF (m a)
  | HaltHF
  | GetSlots (L.List p -> a)
  | RunQuery p (Maybe (f ~> m) -> m a)

instance functorHalogenF :: (Functor f, Functor m) => Functor (HalogenF s f m p) where
  map f = case _ of
    StateHF q -> StateHF (map f q)
    SubscribeHF es a -> SubscribeHF es (f a)
    QueryFHF q -> QueryFHF (map f q)
    QueryGHF q -> QueryGHF (map f q)
    HaltHF -> HaltHF
    GetSlots k -> GetSlots (map f k)
    RunQuery p k -> RunQuery p (map f <$> k)

instance affableHalogenF :: Affable eff m => Affable eff (HalogenF s f m p) where
  fromAff = QueryGHF <<< fromAff

instance altHalogenF :: (Functor f, Functor m) => Alt (HalogenF s f m p) where
  alt HaltHF h = h
  alt h _ = h

instance plusHalogenF :: (Functor f, Functor m) => Plus (HalogenF s f m p) where
  empty = HaltHF

-- | Changes the `g` for a `HalogenF`. Used internally by Halogen.
hoistHalogenF
  :: forall s f m p h
   . Functor h
  => m ~> h
  -> HalogenF s f m p
  ~> HalogenF s f h p
hoistHalogenF eta h =
  case h of
    StateHF q -> StateHF q
    SubscribeHF es next -> SubscribeHF (EventSource (hoistFreeT eta (runEventSource es))) next
    QueryFHF q -> QueryFHF q
    QueryGHF q -> QueryGHF (eta q)
    HaltHF -> HaltHF
    GetSlots k -> GetSlots k
    RunQuery p k -> RunQuery p (PF.dimap (map ?foo) eta k)
