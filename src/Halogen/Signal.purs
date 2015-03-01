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

import Control.Apply (lift2)
import Control.Monad.Eff
    
-- | A `Signal` represents a state machine which responds to inputs of type `i`, producing outputs of type `o`.
newtype Signal eff i o = Signal (i -> Eff eff (Signal1 eff i o))
    
-- | Run a `Signal` by providing an input
runSignal :: forall i o eff. Signal eff i o -> i -> Eff eff (Signal1 eff i o)
runSignal (Signal k) = k
    
-- | `Signal1` represents non-empty signals, i.e. signals with an initial output value.
newtype Signal1 eff i o = Signal1 { result :: o, next :: Signal eff i o }

-- | Run a `Signal1` to obtain the initial value and remaining signal
runSignal1 :: forall eff i o. Signal1 eff i o -> { result :: o, next :: Signal eff i o }
runSignal1 (Signal1 o) = o
  
-- | A `Signal` which returns the latest input
input :: forall eff i. Signal eff i i
input = Signal \i -> return $ Signal1 { result: i, next: input } 

-- | Convert a `Signal` to a `Signal1` by providing an initial value
startingAt :: forall eff i o. Signal eff i o -> o -> Signal1 eff i o
startingAt s o = Signal1 { result: o, next: s }

-- | Get the current value of a `Signal1`
head :: forall eff i o. Signal1 eff i o -> o
head (Signal1 o) = o.result

-- | Convert a `Signal1` to a `Signal` by ignoring its initial value
tail :: forall eff i o. Signal1 eff i o -> Signal eff i o
tail (Signal1 o) = o.next

-- | Creates a stateful `Signal`
stateful :: forall eff s i o. s -> (s -> i -> Eff eff s) -> Signal1 eff i s
stateful s step = go s
  where
  go :: s -> Signal1 eff i s
  go s = Signal1 { result: s, next: Signal \i -> go <$> step s i }

instance functorSignal :: Functor (Signal eff i) where
  (<$>) f (Signal k) = Signal \i -> (f <$>) <$> k i

instance functorSignal1 :: Functor (Signal1 eff i) where
  (<$>) f (Signal1 o) = Signal1 { result: f o.result, next: f <$> o.next }

instance applySignal :: Apply (Signal eff i) where
  (<*>) f x = Signal \i -> runSignal f i `lift2 (<*>)` runSignal x i

instance applySignal1 :: Apply (Signal1 eff i) where
  (<*>) (Signal1 f) (Signal1 x) = Signal1 { result: f.result x.result, next: f.next <*> x.next }

instance applicativeSignal :: Applicative (Signal eff i) where
  pure a = Signal \_ -> pure (pure a)

instance applicativeSignal1 :: Applicative (Signal1 eff i) where
  pure a = Signal1 { result: a, next: pure a }
  
instance profunctorSignal :: Profunctor (Signal eff) where
  dimap f g (Signal k) = Signal \i -> dimap f g <$> k (f i)
  
instance profunctorSignal1 :: Profunctor (Signal1 eff) where
  dimap f g (Signal1 o) = Signal1 { result: g o.result, next: dimap f g o.next }
  
instance semigroupoidSignal :: Semigroupoid (Signal eff) where
  (<<<) f g =
    Signal \i -> do s1 <- runSignal g i
                    s2 <- runSignal f (head s1)
                    return (s2 <<< s1)
         
instance semigroupoidSignal1 :: Semigroupoid (Signal1 eff) where
  (<<<) f g = Signal1 { result: head f, next: tail f <<< tail g }

instance categorySignal :: Category (Signal eff) where
  id = input
 
