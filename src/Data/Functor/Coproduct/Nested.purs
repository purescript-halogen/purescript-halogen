module Data.Functor.Coproduct.Nested where

import Prelude

import Data.Functor.Coproduct
import Data.NaturalTransformation

type Coproduct2 = Coproduct
type Coproduct3 a b = Coproduct (Coproduct2 a b)
type Coproduct4 a b c = Coproduct (Coproduct3 a b c)
type Coproduct5 a b c d = Coproduct (Coproduct4 a b c d)
type Coproduct6 a b c d e = Coproduct (Coproduct5 a b c d e)
type Coproduct7 a b c d e f = Coproduct (Coproduct6 a b c d e f)
type Coproduct8 a b c d e f g = Coproduct (Coproduct7 a b c d e f g)
type Coproduct9 a b c d e f g h = Coproduct (Coproduct8 a b c d e f g h)
type Coproduct10 a b c d e f g h i = Coproduct (Coproduct9 a b c d e f g h i)

-- Coproduct2
coproduct1of2 :: forall a b x. a x -> Coproduct2 a b x
coproduct1of2 = left

coproduct2of2 :: forall a b x. b x -> Coproduct2 a b x
coproduct2of2 = right

-- Coproduct3
coproduct1of3 :: forall a b c x. a x -> Coproduct3 a b c x
coproduct1of3 v = (left (left v))

coproduct2of3 :: forall a b c x. b x -> Coproduct3 a b c x
coproduct2of3 v = (left (right v))

coproduct3of3 :: forall a b c x. c x -> Coproduct3 a b c x
coproduct3of3 v = right v

-- Coproduct4
coproduct1of4 :: forall a b c d x. a x -> Coproduct4 a b c d x
coproduct1of4 v = (left (left (left v)))

coproduct2of4 :: forall a b c d x. b x -> Coproduct4 a b c d x
coproduct2of4 v = (left (left (right v)))

coproduct3of4 :: forall a b c d x. c x -> Coproduct4 a b c d x
coproduct3of4 v = (left (right v))

coproduct4of4 :: forall a b c d x. d x -> Coproduct4 a b c d x
coproduct4of4 v = right v

-- Coproduct5
coproduct1of5 :: forall a b c d e x. a x -> Coproduct5 a b c d e x
coproduct1of5 v = (left (left (left (left v))))

coproduct2of5 :: forall a b c d e x. b x -> Coproduct5 a b c d e x
coproduct2of5 v = (left (left (left (right v))))

coproduct3of5 :: forall a b c d e x. c x -> Coproduct5 a b c d e x
coproduct3of5 v = (left (left (right v)))

coproduct4of5 :: forall a b c d e x. d x -> Coproduct5 a b c d e x
coproduct4of5 v = (left (right v))

coproduct5of5 :: forall a b c d e x. e x -> Coproduct5 a b c d e x
coproduct5of5 v = right v

-- Coproduct6
coproduct1of6 :: forall a b c d e f x. a x -> Coproduct6 a b c d e f x
coproduct1of6 v = (left (left (left (left (left v)))))

coproduct2of6 :: forall a b c d e f x. b x -> Coproduct6 a b c d e f x
coproduct2of6 v = (left (left (left (left (right v)))))

coproduct3of6 :: forall a b c d e f x. c x -> Coproduct6 a b c d e f x
coproduct3of6 v = (left (left (left (right v))))

coproduct4of6 :: forall a b c d e f x. d x -> Coproduct6 a b c d e f x
coproduct4of6 v = (left (left (right v)))

coproduct5of6 :: forall a b c d e f x. e x -> Coproduct6 a b c d e f x
coproduct5of6 v = (left (right v))

coproduct6of6 :: forall a b c d e f x. f x -> Coproduct6 a b c d e f x
coproduct6of6 v = right v

-- Coproduct7
coproduct1of7 :: forall a b c d e f g x. a x -> Coproduct7 a b c d e f g x
coproduct1of7 v = (left (left (left (left (left (left v))))))

coproduct2of7 :: forall a b c d e f g x. b x -> Coproduct7 a b c d e f g x
coproduct2of7 v = (left (left (left (left (left (right v))))))

coproduct3of7 :: forall a b c d e f g x. c x -> Coproduct7 a b c d e f g x
coproduct3of7 v = (left (left (left (left (right v)))))

coproduct4of7 :: forall a b c d e f g x. d x -> Coproduct7 a b c d e f g x
coproduct4of7 v = (left (left (left (right v))))

coproduct5of7 :: forall a b c d e f g x. e x -> Coproduct7 a b c d e f g x
coproduct5of7 v = (left (left (right v)))

coproduct6of7 :: forall a b c d e f g x. f x -> Coproduct7 a b c d e f g x
coproduct6of7 v = (left (right v))

coproduct7of7 :: forall a b c d e f g x. g x -> Coproduct7 a b c d e f g x
coproduct7of7 v = right v

-- Coproduct8
coproduct1of8 :: forall a b c d e f g h x. a x -> Coproduct8 a b c d e f g h x
coproduct1of8 v = (left (left (left (left (left (left (left v)))))))

coproduct2of8 :: forall a b c d e f g h x. b x -> Coproduct8 a b c d e f g h x
coproduct2of8 v = (left (left (left (left (left (left (right v)))))))

coproduct3of8 :: forall a b c d e f g h x. c x -> Coproduct8 a b c d e f g h x
coproduct3of8 v = (left (left (left (left (left (right v))))))

coproduct4of8 :: forall a b c d e f g h x. d x -> Coproduct8 a b c d e f g h x
coproduct4of8 v = (left (left (left (left (right v)))))

coproduct5of8 :: forall a b c d e f g h x. e x -> Coproduct8 a b c d e f g h x
coproduct5of8 v = (left (left (left (right v))))

coproduct6of8 :: forall a b c d e f g h x. f x -> Coproduct8 a b c d e f g h x
coproduct6of8 v = (left (left (right v)))

coproduct7of8 :: forall a b c d e f g h x. g x -> Coproduct8 a b c d e f g h x
coproduct7of8 v = (left (right v))

coproduct8of8 :: forall a b c d e f g h x. h x -> Coproduct8 a b c d e f g h x
coproduct8of8 v = right v

-- Coproduct9
coproduct1of9 :: forall a b c d e f g h i x. a x -> Coproduct9 a b c d e f g h i x
coproduct1of9 v = (left (left (left (left (left (left (left (left v))))))))

coproduct2of9 :: forall a b c d e f g h i x. b x -> Coproduct9 a b c d e f g h i x
coproduct2of9 v = (left (left (left (left (left (left (left (right v))))))))

coproduct3of9 :: forall a b c d e f g h i x. c x -> Coproduct9 a b c d e f g h i x
coproduct3of9 v = (left (left (left (left (left (left (right v)))))))

coproduct4of9 :: forall a b c d e f g h i x. d x -> Coproduct9 a b c d e f g h i x
coproduct4of9 v = (left (left (left (left (left (right v))))))

coproduct5of9 :: forall a b c d e f g h i x. e x -> Coproduct9 a b c d e f g h i x
coproduct5of9 v = (left (left (left (left (right v)))))

coproduct6of9 :: forall a b c d e f g h i x. f x -> Coproduct9 a b c d e f g h i x
coproduct6of9 v = (left (left (left (right v))))

coproduct7of9 :: forall a b c d e f g h i x. g x -> Coproduct9 a b c d e f g h i x
coproduct7of9 v = (left (left (right v)))

coproduct8of9 :: forall a b c d e f g h i x. h x -> Coproduct9 a b c d e f g h i x
coproduct8of9 v = (left (right v))

coproduct9of9 :: forall a b c d e f g h i x. i x -> Coproduct9 a b c d e f g h i x
coproduct9of9 v = right v

-- Coproduct10
coproduct1of10 :: forall a b c d e f g h i j x. a x -> Coproduct10 a b c d e f g h i j x
coproduct1of10 v = (left (left (left (left (left (left (left (left (left v)))))))))

coproduct2of10 :: forall a b c d e f g h i j x. b x -> Coproduct10 a b c d e f g h i j x
coproduct2of10 v = (left (left (left (left (left (left (left (left (right v)))))))))

coproduct3of10 :: forall a b c d e f g h i j x. c x -> Coproduct10 a b c d e f g h i j x
coproduct3of10 v = (left (left (left (left (left (left (left (right v))))))))

coproduct4of10 :: forall a b c d e f g h i j x. d x -> Coproduct10 a b c d e f g h i j x
coproduct4of10 v = (left (left (left (left (left (left (right v)))))))

coproduct5of10 :: forall a b c d e f g h i j x. e x -> Coproduct10 a b c d e f g h i j x
coproduct5of10 v = (left (left (left (left (left (right v))))))

coproduct6of10 :: forall a b c d e f g h i j x. f x -> Coproduct10 a b c d e f g h i j x
coproduct6of10 v = (left (left (left (left (right v)))))

coproduct7of10 :: forall a b c d e f g h i j x. g x -> Coproduct10 a b c d e f g h i j x
coproduct7of10 v = (left (left (left (right v))))

coproduct8of10 :: forall a b c d e f g h i j x. h x -> Coproduct10 a b c d e f g h i j x
coproduct8of10 v = (left (left (right v)))

coproduct9of10 :: forall a b c d e f g h i j x. i x -> Coproduct10 a b c d e f g h i j x
coproduct9of10 v = (left (right v))

coproduct10of10 :: forall a b c d e f g h i j x. j x -> Coproduct10 a b c d e f g h i j x
coproduct10of10 v = right v

coproduct2 :: forall a b z. Natural a z -> Natural b z -> Natural (Coproduct2 a b) z
coproduct2 = coproduct

coproduct3 :: forall a b c z x. Natural a z -> Natural b z -> Natural c z -> Natural (Coproduct3 a b c) z
coproduct3 a b z = coproduct (coproduct2 a b) z

coproduct4 :: forall a b c d z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural (Coproduct4 a b c d) z
coproduct4 a b c z = coproduct (coproduct3 a b c) z

coproduct5 :: forall a b c d e z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural e z -> Natural (Coproduct5 a b c d e) z
coproduct5 a b c d z = coproduct (coproduct4 a b c d) z

coproduct6 :: forall a b c d e f z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural e z -> Natural f z -> Natural (Coproduct6 a b c d e f) z
coproduct6 a b c d e z = coproduct (coproduct5 a b c d e) z

coproduct7 :: forall a b c d e f g z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural e z -> Natural f z -> Natural g z -> Natural (Coproduct7 a b c d e f g) z
coproduct7 a b c d e f z = coproduct (coproduct6 a b c d e f) z

coproduct8 :: forall a b c d e f g h z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural e z -> Natural f z -> Natural g z -> Natural h z -> Natural (Coproduct8 a b c d e f g h) z
coproduct8 a b c d e f g z = coproduct (coproduct7 a b c d e f g) z

coproduct9 :: forall a b c d e f g h i z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural e z -> Natural f z -> Natural g z -> Natural h z -> Natural i z -> Natural (Coproduct9 a b c d e f g h i) z
coproduct9 a b c d e f g h z = coproduct (coproduct8 a b c d e f g h) z

coproduct10 :: forall a b c d e f g h i j z. Natural a z -> Natural b z -> Natural c z -> Natural d z -> Natural e z -> Natural f z -> Natural g z -> Natural h z -> Natural i z -> Natural j z -> Natural (Coproduct10 a b c d e f g h i j) z
coproduct10 a b c d e f g h i z = coproduct (coproduct9 a b c d e f g h i) z
