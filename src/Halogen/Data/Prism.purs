module Halogen.Data.Prism where

import Data.Either (Either)
import Data.Either.Nested as EN
import Data.Functor.Coproduct (Coproduct)
import Data.Functor.Coproduct.Nested as CN
import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))

type Prism1' f g a = Prism' (f a) (g a)

infixr 6 type Either as \/
infixr 6 type Coproduct as <\/>

_Either1 :: forall a z. Prism' (a \/ z) a
_Either1 = prism' EN.in1 (EN.at1 Nothing Just)

_Coproduct1 :: forall a z x. Prism1' (a <\/> z) a x
_Coproduct1 = prism' CN.in1 (CN.at1 Nothing Just)

_Either2 :: forall a b z. Prism' (a \/ b \/ z) b
_Either2 = prism' EN.in2 (EN.at2 Nothing Just)

_Coproduct2 :: forall a b z x. Prism1' (a <\/> b <\/> z) b x
_Coproduct2 = prism' CN.in2 (CN.at2 Nothing Just)

_Either3 :: forall a b c z. Prism' (a \/ b \/ c \/ z) c
_Either3 = prism' EN.in3 (EN.at3 Nothing Just)

_Coproduct3 :: forall a b c z x. Prism1' (a <\/> b <\/> c <\/> z) c x
_Coproduct3 = prism' CN.in3 (CN.at3 Nothing Just)

_Either4 :: forall a b c d z. Prism' (a \/ b \/ c \/ d \/ z) d
_Either4 = prism' EN.in4 (EN.at4 Nothing Just)

_Coproduct4 :: forall a b c d z x. Prism1' (a <\/> b <\/> c <\/> d <\/> z) d x
_Coproduct4 = prism' CN.in4 (CN.at4 Nothing Just)

_Either5 :: forall a b c d e z. Prism' (a \/ b \/ c \/ d \/ e \/ z) e
_Either5 = prism' EN.in5 (EN.at5 Nothing Just)

_Coproduct5 :: forall a b c d z e x. Prism1' (a <\/> b <\/> c <\/> d <\/> e <\/> z) e x
_Coproduct5 = prism' CN.in5 (CN.at5 Nothing Just)

_Either6 :: forall a b c d e f z. Prism' (a \/ b \/ c \/ d \/ e \/ f \/ z) f
_Either6 = prism' EN.in6 (EN.at6 Nothing Just)

_Coproduct6 :: forall a b c d z e f x. Prism1' (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> z) f x
_Coproduct6 = prism' CN.in6 (CN.at6 Nothing Just)

_Either7 :: forall a b c d e f g z. Prism' (a \/ b \/ c \/ d \/ e \/ f \/ g \/ z) g
_Either7 = prism' EN.in7 (EN.at7 Nothing Just)

_Coproduct7 :: forall a b c d z e f g x. Prism1' (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> z) g x
_Coproduct7 = prism' CN.in7 (CN.at7 Nothing Just)

_Either8 :: forall a b c d e f g h z. Prism' (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ z) h
_Either8 = prism' EN.in8 (EN.at8 Nothing Just)

_Coproduct8 :: forall a b c d z e f g h x. Prism1' (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> h <\/> z) h x
_Coproduct8 = prism' CN.in8 (CN.at8 Nothing Just)

_Either9 :: forall a b c d e f g h i z. Prism' (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ z) i
_Either9 = prism' EN.in9 (EN.at9 Nothing Just)

_Coproduct9 :: forall a b c d z e f g h i x. Prism1' (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> h <\/> i <\/> z) i x
_Coproduct9 = prism' CN.in9 (CN.at9 Nothing Just)

_Either10 :: forall a b c d e f g h i j z. Prism' (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h \/ i \/ j \/ z) j
_Either10 = prism' EN.in10 (EN.at10 Nothing Just)

_Coproduct10 :: forall a b c d z e f g h i j x. Prism1' (a <\/> b <\/> c <\/> d <\/> e <\/> f <\/> g <\/> h <\/> i <\/> j <\/> z) j x
_Coproduct10 = prism' CN.in10 (CN.at10 Nothing Just)
