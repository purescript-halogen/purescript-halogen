module Halogen.Signal
  ( SF()
  , SF1()
  
  , runSF
  , runSF1
  
  , arr
  , input
  , stateful
  , stateful'
  , differencesWith
  , loop
  , startingAt
  , head
  , tail
  ) where
    
import Data.Tuple
import Data.Either

import Data.Profunctor
import Data.Profunctor.Strong
import Data.Profunctor.Choice
    
-- | A `SF` represents a state machine which responds to inputs of type `i`, producing outputs of type `o`.
newtype SF i o = SF (i -> SF1 i o)
    
-- | Run a `SF` by providing an input
runSF :: forall i o. SF i o -> i -> SF1 i o
runSF (SF k) = k
    
-- | `SF1` represents non-empty signals, i.e. signals with an initial output value.
newtype SF1 i o = SF1 { result :: o, next :: SF i o }

-- | Run a `SF1` to obtain the initial value and remaining signal
runSF1 :: forall i o. SF1 i o -> { result :: o, next :: SF i o }
runSF1 (SF1 o) = o
  
-- | Create a `SF` from a function  
arr :: forall i o. (i -> o) -> SF i o
arr f = let s = SF \i -> SF1 { result: f i, next: s } in s
  
-- | A `SF` which returns the latest input
input :: forall i. SF i i
input = SF \i -> SF1 { result: i, next: input } 

-- | Convert a `SF` to a `SF1` by providing an initial value
startingAt :: forall i o. SF i o -> o -> SF1 i o
startingAt s o = SF1 { result: o, next: s }

-- | Get the current value of a `SF1`
head :: forall i o. SF1 i o -> o
head (SF1 o) = o.result

-- | Convert a `SF1` to a `SF` by ignoring its initial value
tail :: forall i o. SF1 i o -> SF i o
tail (SF1 o) = o.next

-- | Creates a stateful `SF1`
stateful :: forall s i o. s -> (s -> i -> s) -> SF1 i s
stateful s step = stateful' s (\s i -> let s' = step s i in Tuple s' s') `startingAt` s
  
-- | Creates a stateful `SF` based on a function which returns an output value
stateful' :: forall s i o. s -> (s -> i -> Tuple o s) -> SF i o
stateful' s step = go s
  where
  go :: s -> SF i o
  go s = SF \i -> 
    case step s i of
      Tuple o s' -> SF1 { result: o, next: go s' }
      
-- | A `SF` which compares consecutive inputs using a helper function
differencesWith :: forall i d. (i -> i -> d) -> i -> SF i d
differencesWith f initial = stateful' initial \last next -> 
  let d = f last next 
  in Tuple d next
  
-- | Create a `SF` which hides a piece of internal state of type `s`.
loop :: forall s i o. s -> SF (Tuple s i) (Tuple s o) -> SF i o
loop s signal = SF \i -> 
  case runSF signal (Tuple s i) of
    SF1 o -> SF1 { result: snd (o.result), next: loop (fst o.result) o.next }

instance functorSF :: Functor (SF i) where
  (<$>) f (SF k) = SF \i -> f <$> k i

instance functorSF1 :: Functor (SF1 i) where
  (<$>) f (SF1 o) = SF1 { result: f o.result, next: f <$> o.next }

instance applySF :: Apply (SF i) where
  (<*>) f x = SF \i -> runSF f i <*> runSF x i

instance applySF1 :: Apply (SF1 i) where
  (<*>) (SF1 f) (SF1 x) = SF1 { result: f.result x.result, next: f.next <*> x.next }

instance applicativeSF :: Applicative (SF i) where
  pure a = SF \_ -> pure a

instance applicativeSF1 :: Applicative (SF1 i) where
  pure a = SF1 { result: a, next: pure a }
  
instance profunctorSF :: Profunctor SF where
  dimap f g (SF k) = SF \i -> dimap f g (k (f i))
  
instance profunctorSF1 :: Profunctor SF1 where
  dimap f g (SF1 o) = SF1 { result: g o.result, next: dimap f g o.next }
  
instance strongSF :: Strong SF where
  first s = SF \(Tuple a c) -> 
    case runSF s a of
      SF1 o -> SF1 { result: Tuple o.result c, next: first o.next }
  second s = SF \(Tuple a b) -> 
    case runSF s b of
      SF1 o -> SF1 { result: Tuple a o.result, next: second o.next }
  
instance choiceSF :: Choice SF where
  left s = SF \e -> 
    case e of
      Left a -> case runSF s a of
                  SF1 o -> SF1 { result: Left o.result, next: left o.next }
      Right c -> SF1 { result: Right c, next: left s }
  right s = SF \e -> 
    case e of
      Left a -> SF1 { result: Left a, next: right s }
      Right b -> case runSF s b of
                   SF1 o -> SF1 { result: Right o.result, next: right o.next }

instance semigroupoidSF :: Semigroupoid SF where
  (<<<) f g =
    SF \i -> let s1 = runSF g i
                 s2 = runSF f (head s1)
             in s2 <<< s1
         
instance semigroupoidSF1 :: Semigroupoid SF1 where
  (<<<) f g = SF1 { result: head f, next: tail f <<< tail g }

instance categorySF :: Category SF where
  id = input
