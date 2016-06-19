module Halogen.Component.ChildPath where

import Prelude

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Injector (Injector, prj, inj, injI, injLE, injLC, injRE, injRC)
import Data.Maybe (Maybe)

-- | Represents a path through `Either` and `Coproduct` types for the state,
-- | query algebra, and slots of a component. Used when installing children of
-- | different types within a single parent component.
data ChildPath s s' f f' p p' = ChildPath (Injector s s') (forall a. Injector (f a) (f' a)) (Injector p p')

-- | Composes two paths.
compose :: forall s t u f g h p q r. ChildPath t u g h q r -> ChildPath s t f g p q -> ChildPath s u f h p r
compose (ChildPath f h j) (ChildPath g i k) = ChildPath (f <<< g) (h <<< i) (j <<< k)

infixl 4 compose as :>

-- | A `ChildPath` that represents taking the left-hand choice for each of a
-- | component's `Either` and `Coproduct`s choices.
cpL :: forall s t f g p q. ChildPath s (Either s t) f (Coproduct f g) p (Either p q)
cpL = ChildPath injLE injLC injLE

-- | A `ChildPath` that represents taking the right-hand choice for each of a
-- | component's `Either` and `Coproduct`s choices.
cpR :: forall s t f g p q. ChildPath s (Either t s) f (Coproduct g f) p (Either q p)
cpR = ChildPath injRE injRC injRE

-- | An identity `ChildPath`.
cpI :: forall s f p. ChildPath s s f f p p
cpI = ChildPath injI injI injI

-- | Uses a `ChildPath` definition to get a state value of type `s'` from a
-- | value of type `s`. Used internally by Halogen.
injState :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> s -> s'
injState (ChildPath injS _ _) = inj injS

-- | Uses a `ChildPath` to attempt to get a state value of type `s` from a value
-- | of type `s'`. Used internally by Halogen.
prjState :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> s' -> Maybe s
prjState (ChildPath injS _ _) = prj injS

-- | Uses a `ChildPath` definition to get a query algebra value of type `f'`
-- | from a value of type `f`. Used internally by Halogen.
injQuery :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> f ~> f'
injQuery (ChildPath _ injQ _) = inj injQ

-- | Uses a `ChildPath` to attempt to get a query algebra value of type `f`
-- | from a value of type `f'`. Used internally by Halogen.
prjQuery :: forall s s' f f' p p' a. ChildPath s s' f f' p p' -> f' a -> Maybe (f a)
prjQuery (ChildPath _ injQ _) = prj injQ

-- | Uses a `ChildPath` definition to get a slot value of type `p'` from a
-- | value of type `p`. Used internally by Halogen.
injSlot :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> p -> p'
injSlot (ChildPath _ _ injP) = inj injP

-- | Uses a `ChildPath` definition to get a slot value of type `p'` from a
-- | value of type `p`. Used internally by Halogen.
prjSlot :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> p' -> Maybe p
prjSlot (ChildPath _ _ injP) = prj injP
