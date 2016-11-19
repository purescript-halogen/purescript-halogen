module Halogen.Data.Injector
  ( Prism
  , Injector
  , type (\/), type (<\/>)
  , inj
  , prj
  , injI
  , injLE
  , injLC
  , injRE
  , injRC
  , injE1, injC1
  , injE2, injC2
  , injE3, injC3
  , injE4, injC4
  , injE5, injC5
  , injE6, injC6
  , injE7, injC7
  , injE8, injC8
  , injE9, injC9
  , injE10, injC10
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Either (Either(..), either)
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (unwrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Choice as PF
import Data.Either.Nested as EN
import Data.Functor.Coproduct.Nested as CN

-- | Compatible with `Prism` from `purescript-profunctor-lenses`.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
prism f g = dimap g (either pure (map f)) <<< PF.right

prism' :: forall s a b. (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' f g = prism f (\s -> maybe (Left s) Right (g s))

-- | Compatible with `Prism'` from `purescript-profunctor-lenses`.
type Injector s a = Prism a a s s
type Injector1 f g a = Injector (f a) (g a)

newtype Tagged s b = Tagged b

instance profunctorTagged :: Profunctor Tagged where
  dimap _ f (Tagged b) = Tagged (f b)

instance choiceTagged :: Choice Tagged where
  left (Tagged b) = Tagged (Left b)
  right (Tagged b) = Tagged (Right b)

unTagged :: forall s b. Tagged s b -> b
unTagged (Tagged b) = b

inj :: forall a b. Injector a b -> a -> b
inj p = unwrap <<< unTagged <<< p <<< Tagged <<< Identity

prj :: forall a b. Injector a b -> b -> Maybe a
prj p = unwrap <<< unwrap <<< p (Const <<< First <<< Just)

injI :: forall a. Injector a a
injI = prism' id Just

injLE :: forall a b. Injector a (Either a b)
injLE = prism' Left (either Just (const Nothing))

injRE :: forall a b. Injector a (Either b a)
injRE = prism' Right (either (const Nothing) Just)

injLC :: forall f g a. Injector (f a) (Coproduct f g a)
injLC = prism' left (coproduct Just (const Nothing))

injRC :: forall f g a. Injector (f a) (Coproduct g f a)
injRC = prism' right (coproduct (const Nothing) Just)

infixr 6 type Either as \/
infixr 6 type Coproduct as <\/>

injE1 :: forall a z. Injector a (a \/ z)
injE1 = prism' EN.in1 (EN.at1 Nothing Just)

injC1 :: forall a z x. Injector1 a (a <\/> z) x
injC1 = prism' CN.in1 (CN.at1 Nothing Just)

injE2 :: forall a b z. Injector b (a \/ b \/ z)
injE2 = prism' EN.in2 (EN.at2 Nothing Just)

injC2 :: forall a b z x. Injector1 b (a <\/> b <\/> z) x
injC2 = prism' CN.in2 (CN.at2 Nothing Just)

injE3 :: forall a b c z. Injector c (a \/ b \/ c \/ z)
injE3 = prism' EN.in3 (EN.at3 Nothing Just)

injC3 :: forall a b c z x. Injector1 c (a <\/> b <\/> c <\/> z) x
injC3 = prism' CN.in3 (CN.at3 Nothing Just)

injE4 :: forall a b c d z. Injector d (a \/ b \/ c \/ d \/ z)
injE4 = prism' EN.in4 (EN.at4 Nothing Just)

injC4 :: forall a b c d z x. Injector1 d (a <\/> b <\/> c <\/> d <\/> z) x
injC4 = prism' CN.in4 (CN.at4 Nothing Just)

injE5 :: forall a b c d e z. Injector e (a \/ b \/ c \/ d \/ e \/ z)
injE5 = prism' EN.in5 (EN.at5 Nothing Just)

injC5 :: forall a b c d z e x. Injector1 e (a <\/> b <\/> c <\/> d <\/> e <\/> z) x
injC5 = prism' CN.in5 (CN.at5 Nothing Just)

injE6 :: forall a b c d e f z. Injector f (a \/ b \/ c \/ d \/ e \/ f \/ z)
injE6 = prism' EN.in6 (EN.at6 Nothing Just)

injC6 :: forall a b c d z e f x. Injector1 f (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> z) x
injC6 = prism' CN.in6 (CN.at6 Nothing Just)

injE7 :: forall a b c d e f g z. Injector g (a \/ b \/ c \/ d \/ e \/ f \/ g \/ z)
injE7 = prism' EN.in7 (EN.at7 Nothing Just)

injC7 :: forall a b c d z e f g x. Injector1 g (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> z) x
injC7 = prism' CN.in7 (CN.at7 Nothing Just)

injE8 :: forall a b c d e f g h z. Injector h (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ z)
injE8 = prism' EN.in8 (EN.at8 Nothing Just)

injC8 :: forall a b c d z e f g h x. Injector1 h (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> h <\/> z) x
injC8 = prism' CN.in8 (CN.at8 Nothing Just)

injE9 :: forall a b c d e f g h i z. Injector i (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ z)
injE9 = prism' EN.in9 (EN.at9 Nothing Just)

injC9 :: forall a b c d z e f g h i x. Injector1 i (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> h <\/> i <\/> z) x
injC9 = prism' CN.in9 (CN.at9 Nothing Just)

injE10 :: forall a b c d e f g h i j z. Injector j (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ z)
injE10 = prism' EN.in10 (EN.at10 Nothing Just)

injC10 :: forall a b c d z e f g h i j x. Injector1 j (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> h <\/> i <\/> j <\/> z) x
injC10 = prism' CN.in10 (CN.at10 Nothing Just)
