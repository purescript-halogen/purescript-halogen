module Halogen.Query.StateF
  ( StateF(..)
  , get
  , gets
  , modify
  , stateN
  ) where

import Prelude

import Control.Monad.Free (Free(), liftF)
import qualified Control.Monad.State as CMS
import qualified Control.Monad.State.Class as CMS

import Data.Coyoneda (Natural())
import Data.Functor (($>))
import Data.Inject (Inject, inj)

data StateF s a
  = Get (s -> a)
  | Modify (s -> s) a

instance functorStateF :: Functor (StateF s) where
  map f (Get k) = Get (f <<< k)
  map f (Modify g next) = Modify g (f next)

get :: forall f s. (Inject (StateF s) f, Functor f) => Free f s
get = gets id

gets :: forall f s a. (Inject (StateF s) f, Functor f) => (s -> a) -> Free f a
gets f = liftF $ inj (Get f) :: f a

modify :: forall f s. (Inject (StateF s) f, Functor f) => (s -> s) -> Free f Unit
modify f = liftF $ inj (Modify f unit) :: f Unit

stateN :: forall s m. (Monad m, CMS.MonadState s m) => Natural (StateF s) m
stateN (Get k) = CMS.get >>= pure <<< k
stateN (Modify f next) = CMS.modify f $> next
