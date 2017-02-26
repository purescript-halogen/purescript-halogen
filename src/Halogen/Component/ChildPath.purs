module Halogen.Component.ChildPath
  ( module Halogen.Component.ChildPath
  , module Halogen.Data.Prism
  ) where

import Prelude hiding (compose)

import Data.Lens (Prism', review, preview)
import Data.Lens.Prism.Coproduct as PC
import Data.Lens.Prism.Either as PE
import Data.Maybe (Maybe)

import Halogen.Data.Prism (type (<\/>), type (\/), _Coproduct1, _Coproduct10, _Coproduct2, _Coproduct3, _Coproduct4, _Coproduct5, _Coproduct6, _Coproduct7, _Coproduct8, _Coproduct9, _Either1, _Either10, _Either2, _Either3, _Either4, _Either5, _Either6, _Either7, _Either8, _Either9)

-- | Represents a path through `Either` and `Coproduct` types for the state,
-- | query algebra, and slots of a component. Used when installing children of
-- | different types within a single parent component.
data ChildPath f f' p p' = ChildPath (forall a. Prism' (f' a) (f a)) (Prism' p' p)

-- | Composes two paths.
compose :: forall f g h p q r. ChildPath g h q r -> ChildPath f g p q -> ChildPath f h p r
compose (ChildPath f h) (ChildPath g i) = ChildPath (f <<< g) (h <<< i)

infixl 4 compose as :>

-- | An identity `ChildPath`.
cpI :: forall f p. ChildPath f f p p
cpI = ChildPath id id

-- | A `ChildPath` that goes to the left.
cpL :: forall f g p q. ChildPath f (f <\/> g) p (p \/ q)
cpL = ChildPath PC._Left PE._Left

-- | A `ChildPath` that goes to the right.
cpR :: forall f g p q. ChildPath f (g <\/> f) p (q \/ p)
cpR = ChildPath PC._Right PE._Right

-- | A `ChildPath` for the 1st component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp1 :: forall f1 g p1 q. ChildPath f1 (f1 <\/> g) p1 (p1 \/ q)
cp1 = ChildPath _Coproduct1 _Either1

-- | A `ChildPath` for the 2nd component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp2 :: forall f1 f2 g p1 p2 q. ChildPath f2 (f1 <\/> f2 <\/> g) p2 (p1 \/ p2 \/ q)
cp2 = ChildPath _Coproduct2 _Either2

-- | A `ChildPath` for the 3rd component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp3 :: forall f1 f2 f3 g p1 p2 p3 q. ChildPath f3 (f1 <\/> f2 <\/> f3 <\/> g) p3 (p1 \/ p2 \/ p3 \/ q)
cp3 = ChildPath _Coproduct3 _Either3

-- | A `ChildPath` for the 4th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp4 :: forall f1 f2 f3 f4 g p1 p2 p3 p4 q. ChildPath f4 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> g) p4 (p1 \/ p2 \/ p3 \/ p4 \/ q)
cp4 = ChildPath _Coproduct4 _Either4

-- | A `ChildPath` for the 5th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp5 :: forall f1 f2 f3 f4 f5 g p1 p2 p3 p4 p5 q. ChildPath f5 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> g) p5 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ q)
cp5 = ChildPath _Coproduct5 _Either5

-- | A `ChildPath` for the 6th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp6 :: forall f1 f2 f3 f4 f5 f6 g p1 p2 p3 p4 p5 p6 q. ChildPath f6 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> g) p6 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ q)
cp6 = ChildPath _Coproduct6 _Either6

-- | A `ChildPath` for the 7th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp7 :: forall f1 f2 f3 f4 f5 f6 f7 g p1 p2 p3 p4 p5 p6 p7 q. ChildPath f7 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> g) p7 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ q)
cp7 = ChildPath _Coproduct7 _Either7

-- | A `ChildPath` for the 8th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp8 :: forall f1 f2 f3 f4 f5 f6 f7 f8 g p1 p2 p3 p4 p5 p6 p7 p8 q. ChildPath f8 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> f8 <\/> g) p8 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ p8 \/ q)
cp8 = ChildPath _Coproduct8 _Either8

-- | A `ChildPath` for the 9th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp9 :: forall f1 f2 f3 f4 f5 f6 f7 f8 f9 g p1 p2 p3 p4 p5 p6 p7 p8 p9 q. ChildPath f9 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> f8 <\/> f9 <\/> g) p9 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ p8 \/ p9 \/ q)
cp9 = ChildPath _Coproduct9 _Either9

-- | A `ChildPath` for the 10th component defined using a `Either.Nested` and
-- | `Coproduct.Nested` representation for the sum.
cp10 :: forall f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 g p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 q. ChildPath f10 (f1 <\/> f2 <\/> f3 <\/> f4 <\/> f5 <\/> f6 <\/> f7 <\/> f8 <\/> f9 <\/> f10 <\/> g) p10 (p1 \/ p2 \/ p3 \/ p4 \/ p5 \/ p6 \/ p7 \/ p8 \/ p9 \/ p10 \/ q)
cp10 = ChildPath _Coproduct10 _Either10

-- | Uses a `ChildPath` definition to get a query algebra value of type `f'`
-- | from a value of type `f`. Used internally by Halogen.
injQuery :: forall f f' p p'. ChildPath f f' p p' -> f ~> f'
injQuery (ChildPath injQ _) = review injQ

-- | Uses a `ChildPath` to attempt to get a query algebra value of type `f`
-- | from a value of type `f'`. Used internally by Halogen.
prjQuery :: forall f f' p p' a. ChildPath f f' p p' -> f' a -> Maybe (f a)
prjQuery (ChildPath injQ _) = preview injQ

-- | Uses a `ChildPath` definition to get a slot value of type `p'` from a
-- | value of type `p`. Used internally by Halogen.
injSlot :: forall f f' p p'. ChildPath f f' p p' -> p -> p'
injSlot (ChildPath _ injP) = review injP

-- | Uses a `ChildPath` definition to get a slot value of type `p'` from a
-- | value of type `p`. Used internally by Halogen.
prjSlot :: forall f f' p p'. ChildPath f f' p p' -> p' -> Maybe p
prjSlot (ChildPath _ injP) = preview injP
