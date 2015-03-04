module Halogen.Signal 
  ( Signal()
  , Signal1()
  
  , runSignal
  , runSignal1
  
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
    
-- | A `Signal` represents a state machine which responds to inputs of type `i`, producing outputs of type `o`.
newtype Signal i o = Signal (i -> Signal1 i o)
    
-- | Run a `Signal` by providing an input
runSignal :: forall i o. Signal i o -> i -> Signal1 i o
runSignal (Signal k) = k
    
-- | `Signal1` represents non-empty signals, i.e. signals with an initial output value.
newtype Signal1 i o = Signal1 { result :: o, next :: Signal i o }

-- | Run a `Signal1` to obtain the initial value and remaining signal
runSignal1 :: forall i o. Signal1 i o -> { result :: o, next :: Signal i o }
runSignal1 (Signal1 o) = o
  
-- | Create a `Signal` from a function  
arr :: forall i o. (i -> o) -> Signal i o
arr f = let s = Signal \i -> Signal1 { result: f i, next: s } in s
  
-- | A `Signal` which returns the latest input
input :: forall i. Signal i i
input = Signal \i -> Signal1 { result: i, next: input } 

-- | Convert a `Signal` to a `Signal1` by providing an initial value
startingAt :: forall i o. Signal i o -> o -> Signal1 i o
startingAt s o = Signal1 { result: o, next: s }

-- | Get the current value of a `Signal1`
head :: forall i o. Signal1 i o -> o
head (Signal1 o) = o.result

-- | Convert a `Signal1` to a `Signal` by ignoring its initial value
tail :: forall i o. Signal1 i o -> Signal i o
tail (Signal1 o) = o.next

-- | Creates a stateful `Signal1`
stateful :: forall s i o. s -> (s -> i -> s) -> Signal1 i s
stateful s step = stateful' s (\s i -> let s' = step s i in Tuple s' s') `startingAt` s
  
-- | Creates a stateful `Signal` based on a function which returns an output value
stateful' :: forall s i o. s -> (s -> i -> Tuple o s) -> Signal i o
stateful' s step = go s
  where
  go :: s -> Signal i o
  go s = Signal \i -> 
    case step s i of
      Tuple o s' -> Signal1 { result: o, next: go s' }
      
-- | A `Signal` which compares consecutive inputs using a helper function
differencesWith :: forall i d. (i -> i -> d) -> i -> Signal i d
differencesWith f initial = stateful' initial \last next -> 
  let d = f last next 
  in Tuple d next
  
-- | Create a `Signal` which hides a piece of internal state of type `s`.
loop :: forall s i o. s -> Signal (Tuple s i) (Tuple s o) -> Signal i o
loop s signal = Signal \i -> 
  case runSignal signal (Tuple s i) of
    Signal1 o -> Signal1 { result: snd (o.result), next: loop (fst o.result) o.next }

instance functorSignal :: Functor (Signal i) where
  (<$>) f (Signal k) = Signal \i -> f <$> k i

instance functorSignal1 :: Functor (Signal1 i) where
  (<$>) f (Signal1 o) = Signal1 { result: f o.result, next: f <$> o.next }

instance applySignal :: Apply (Signal i) where
  (<*>) f x = Signal \i -> runSignal f i <*> runSignal x i

instance applySignal1 :: Apply (Signal1 i) where
  (<*>) (Signal1 f) (Signal1 x) = Signal1 { result: f.result x.result, next: f.next <*> x.next }

instance applicativeSignal :: Applicative (Signal i) where
  pure a = Signal \_ -> pure a

instance applicativeSignal1 :: Applicative (Signal1 i) where
  pure a = Signal1 { result: a, next: pure a }
  
instance profunctorSignal :: Profunctor Signal where
  dimap f g (Signal k) = Signal \i -> dimap f g (k (f i))
  
instance profunctorSignal1 :: Profunctor Signal1 where
  dimap f g (Signal1 o) = Signal1 { result: g o.result, next: dimap f g o.next }
  
instance strongSignal :: Strong Signal where
  first s = Signal \(Tuple a c) -> 
    case runSignal s a of
      Signal1 o -> Signal1 { result: Tuple o.result c, next: first o.next }
  second s = Signal \(Tuple a b) -> 
    case runSignal s b of
      Signal1 o -> Signal1 { result: Tuple a o.result, next: second o.next }
  
instance choiceSignal :: Choice Signal where
  left s = Signal \e -> 
    case e of
      Left a -> case runSignal s a of
                  Signal1 o -> Signal1 { result: Left o.result, next: left o.next }
      Right c -> Signal1 { result: Right c, next: left s }
  right s = Signal \e -> 
    case e of
      Left a -> Signal1 { result: Left a, next: right s }
      Right b -> case runSignal s b of
                  Signal1 o -> Signal1 { result: Right o.result, next: right o.next }

instance semigroupoidSignal :: Semigroupoid Signal where
  (<<<) f g =
    Signal \i -> let s1 = runSignal g i
                     s2 = runSignal f (head s1)
                 in s2 <<< s1
         
instance semigroupoidSignal1 :: Semigroupoid Signal1 where
  (<<<) f g = Signal1 { result: head f, next: tail f <<< tail g }

instance categorySignal :: Category Signal where
  id = input
