module Halogen.Component.Inject where

import Prelude
import Control.Bind ((<=<))
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Profunctor
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (Natural())

data Inject a b = Inject (a -> b) (b -> Maybe a)

instance semigroupoidInject :: Semigroupoid Inject where
  compose (Inject injA prjA) (Inject injB prjB) = Inject (injA <<< injB) (prjB <=< prjA)

instance categoryInject :: Category Inject where
  id = Inject id Just

inj :: forall a b. Inject a b -> a -> b
inj (Inject f _) = f

prj :: forall a b. Inject a b -> b -> Maybe a
prj (Inject _ g) = g

injLE :: forall a b. Inject a (Either a b)
injLE = Inject Left (either Just (const Nothing))

injRE :: forall a b. Inject a (Either b a)
injRE = Inject Right (either (const Nothing) Just)

injLC :: forall f g a. Inject (f a) (Coproduct f g a)
injLC = Inject left (coproduct Just (const Nothing))

injRC :: forall f g a. Inject (f a) (Coproduct g f a)
injRC = Inject right (coproduct (const Nothing) Just)

data InjectC s s' f f' p p' = InjectC (Inject s s') (forall a. Inject (f a) (f' a)) (Inject p p')

injState :: forall s s' f f' p p'. InjectC s s' f f' p p' -> s -> s'
injState (InjectC injS _ _) = inj injS

prjState :: forall s s' f f' p p'. InjectC s s' f f' p p' -> s' -> Maybe s
prjState (InjectC injS _ _) = prj injS

injQuery :: forall s s' f f' p p'. InjectC s s' f f' p p' -> Natural f f'
injQuery (InjectC _ injQ _) = inj injQ

prjQuery :: forall s s' f f' p p' a. InjectC s s' f f' p p' -> f' a -> Maybe (f a)
prjQuery (InjectC _ injQ _) = prj injQ

injSlot :: forall s s' f f' p p'. InjectC s s' f f' p p' -> p -> p'
injSlot (InjectC _ _ injP) = inj injP

prjSlot :: forall s s' f f' p p'. InjectC s s' f f' p p' -> p' -> Maybe p
prjSlot (InjectC _ _ injP) = prj injP

composeInjC :: forall s t u f g h p q r. InjectC t u g h q r -> InjectC s t f g p q -> InjectC s u f h p r
composeInjC (InjectC f h j) (InjectC g i k) = InjectC (f <<< g) (h <<< i) (j <<< k)

(:>) :: forall s t u f g h p q r. InjectC t u g h q r -> InjectC s t f g p q -> InjectC s u f h p r
(:>) = composeInjC

inl :: forall s t f g p q. InjectC s (Either s t) f (Coproduct f g) p (Either p q)
inl = InjectC injLE injLC injLE

inr :: forall s t f g p q. InjectC s (Either t s) f (Coproduct g f) p (Either q p)
inr = InjectC injRE injRC injRE
