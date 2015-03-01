module Halogen.Signal 
  ( Signal()
  , Signal1()
  
  , runSignal
  , runSignal1
  
  , input
  , stateful
  
  , startingAt
  
  , head
  , tail
  ) where
      
import Data.Profunctor
    
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

-- | Creates a stateful `Signal`
stateful :: forall s i o. s -> (s -> i -> s) -> Signal1 i s
stateful s step = go s
  where
  go :: s -> Signal1 i s
  go s = Signal1 { result: s, next: Signal (go <<< step s) }

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
  
instance semigroupoidSignal :: Semigroupoid Signal where
  (<<<) f g =
    Signal \i -> let s1 = runSignal g i
                     s2 = runSignal f (head s1)
                 in (tail s2 <<< tail s1) `startingAt` head s2
                 
instance categorySignal :: Category Signal where
  id = input
  