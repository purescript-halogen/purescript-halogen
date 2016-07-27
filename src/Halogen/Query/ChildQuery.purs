module Halogen.Query.ChildQuery
  ( ChildQuery
  , ChildQueryF(..)
  , mkChildQuery
  , unChildQuery
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Free (Free, liftF)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Plus (class Plus)

import Data.Coyoneda
import Data.Const (Const)
import Data.List as L
import Data.Leibniz
import Data.Maybe (Maybe(..))
import Data.Profunctor as PF

import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.StateF (StateF)
import Unsafe.Coerce (unsafeCoerce)

newtype ChildQueryF g o i = ChildQueryF (Coyoneda g i)

mapChildQueryF
  :: forall a b g
   . (a -> b)
  -> ChildQueryF g (Maybe a) a
  -> ChildQueryF g (Maybe b) b
mapChildQueryF f (ChildQueryF q) = ChildQueryF (map f q)

data ChildQuery (g :: * -> *) a

instance functorChildQuery :: Functor (ChildQuery g) where
  map f _ = unsafeCoerce unit

mkChildQuery :: forall g a. ChildQueryF g (Maybe a) a -> ChildQuery g (Maybe a)
mkChildQuery = unsafeCoerce

unChildQuery :: forall g a. ChildQuery g (Maybe a) -> ChildQueryF g (Maybe a) a
unChildQuery = unsafeCoerce
