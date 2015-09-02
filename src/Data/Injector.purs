module Data.Injector where

import Prelude (Semigroupoid, Category, (<<<), const, id)

import Control.Bind ((<=<))

import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Maybe (Maybe(..))

data Injector a b = Injector (a -> b) (b -> Maybe a)

instance semigroupoidInjector :: Semigroupoid Injector where
  compose (Injector injA prjA) (Injector injB prjB) = Injector (injA <<< injB) (prjB <=< prjA)

instance categoryInjector :: Category Injector where
  id = Injector id Just

inj :: forall a b. Injector a b -> a -> b
inj (Injector f _) = f

prj :: forall a b. Injector a b -> b -> Maybe a
prj (Injector _ g) = g

injLE :: forall a b. Injector a (Either a b)
injLE = Injector Left (either Just (const Nothing))

injRE :: forall a b. Injector a (Either b a)
injRE = Injector Right (either (const Nothing) Just)

injLC :: forall f g a. Injector (f a) (Coproduct f g a)
injLC = Injector left (coproduct Just (const Nothing))

injRC :: forall f g a. Injector (f a) (Coproduct g f a)
injRC = Injector right (coproduct (const Nothing) Just)
