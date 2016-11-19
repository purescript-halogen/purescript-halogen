module Halogen.Component.ChildPath
  ( module Halogen.Component.ChildPath
  , module Halogen.Data.Injector
  ) where

import Prelude hiding (compose)

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe)
import Halogen.Data.Injector (type (\/), type (<\/>), Injector, prj, inj, injE10, injC10, injE9, injC9, injE8, injC8, injE7, injC7, injE6, injC6, injE5, injC5, injE4, injC4, injE3, injC3, injE2, injC2, injE1, injC1, injI, injRE, injRC, injLE, injLC)

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

-- | A `ChildPath` for the 1st component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp1 :: forall f1 g p1 q. ChildPath f1 (f1 <\/> g) p1 (p1 \/ q)
cp1 = ChildPath injC1 injE1

-- | A `ChildPath` for the 2nd component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp2 :: forall f1 f2 g p1 p2 q. ChildPath f2 (f1 <\/> f2 <\/> g) p2 (p1 \/ p2 \/ q)
cp2 = ChildPath injC2 injE2

-- | A `ChildPath` for the 3rd component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp3 :: forall f1 f2 f3 g p1 p2 p3 q. ChildPath f3 (f1 <\/> f2 <\/> f3 <\/> g) p3 (p1 \/ p2 \/ p3 \/ q)
cp3 = ChildPath injC3 injE3

-- | A `ChildPath` for the 4th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp4 :: forall f1 f2 f3 f4 g p1 p2 p3 p4 q. ChildPath f4 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> g) p4 (p1 \/ p2 \/ p3 \/ p4 \/ q)
cp4 = ChildPath injC4 injE4

-- | A `ChildPath` for the 5th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp5 :: forall f1 f2 f3 f4 f5 g p1 p2 p3 p4 p5 q. ChildPath f5 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> g) p5 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ q)
cp5 = ChildPath injC5 injE5

-- | A `ChildPath` for the 6th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp6 :: forall f1 f2 f3 f4 f5 f6 g p1 p2 p3 p4 p5 p6 q. ChildPath f6 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> g) p6 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ q)
cp6 = ChildPath injC6 injE6

-- | A `ChildPath` for the 7th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp7 :: forall f1 f2 f3 f4 f5 f6 f7 g p1 p2 p3 p4 p5 p6 p7 q. ChildPath f7 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> g) p7 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ q)
cp7 = ChildPath injC7 injE7

-- | A `ChildPath` for the 8th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp8 :: forall f1 f2 f3 f4 f5 f6 f7 f8 g p1 p2 p3 p4 p5 p6 p7 p8 q. ChildPath f8 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> f8 <\/> g) p8 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ p8 \/ q)
cp8 = ChildPath injC8 injE8

-- | A `ChildPath` for the 9th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp9 :: forall f1 f2 f3 f4 f5 f6 f7 f8 f9 g p1 p2 p3 p4 p5 p6 p7 p8 p9 q. ChildPath f9 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> f8 <\/> f9 <\/> g) p9 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ p8 \/ p9 \/ q)
cp9 = ChildPath injC9 injE9

-- | A `ChildPath` for the 10th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp10 :: forall f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 g p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 q. ChildPath f10 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> f8 <\/> f9 <\/> f10 <\/> g) p10 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ p8 \/ p9 \/ p10 \/ q)
cp10 = ChildPath injC10 injE10

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
