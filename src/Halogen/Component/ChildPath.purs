module Halogen.Component.ChildPath where

import Prelude hiding (compose)

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Injector (Injector, prj, inj, injI, injLE, injLC, injRE, injRC)
import Data.Maybe (Maybe)

-- | Represents a path through `Either` and `Coproduct` types for the state,
-- | query algebra, and slots of a component. Used when installing children of
-- | different types within a single parent component.
data ChildPath f f' p p' = ChildPath (forall a. Injector (f a) (f' a)) (Injector p p')

-- | Composes two paths.
compose :: forall f g h p q r. ChildPath g h q r -> ChildPath f g p q -> ChildPath f h p r
compose (ChildPath f h) (ChildPath g i) = ChildPath (f <<< g) (h <<< i)

infixl 4 compose as :>

-- | A `ChildPath` that represents taking the left-hand choice for each of a
-- | component's `Either` and `Coproduct`s choices.
cpL :: forall f g p q. ChildPath f (Coproduct f g) p (Either p q)
cpL = ChildPath injLC injLE

-- | A `ChildPath` that represents taking the right-hand choice for each of a
-- | component's `Either` and `Coproduct`s choices.
cpR :: forall f g p q. ChildPath f (Coproduct g f) p (Either q p)
cpR = ChildPath injRC injRE

-- | An identity `ChildPath`.
cpI :: forall f p. ChildPath f f p p
cpI = ChildPath injI injI

-- | Uses a `ChildPath` definition to get a query algebra value of type `f'`
-- | from a value of type `f`. Used internally by Halogen.
injQuery :: forall f f' p p'. ChildPath f f' p p' -> f ~> f'
injQuery (ChildPath injQ _) = inj injQ

-- | Uses a `ChildPath` to attempt to get a query algebra value of type `f`
-- | from a value of type `f'`. Used internally by Halogen.
prjQuery :: forall f f' p p' a. ChildPath f f' p p' -> f' a -> Maybe (f a)
prjQuery (ChildPath injQ _) = prj injQ

-- | Uses a `ChildPath` definition to get a slot value of type `p'` from a
-- | value of type `p`. Used internally by Halogen.
injSlot :: forall f f' p p'. ChildPath f f' p p' -> p -> p'
injSlot (ChildPath _ injP) = inj injP

-- | Uses a `ChildPath` definition to get a slot value of type `p'` from a
-- | value of type `p`. Used internally by Halogen.
prjSlot :: forall f f' p p'. ChildPath f f' p p' -> p' -> Maybe p
prjSlot (ChildPath _ injP) = prj injP
