-- | This module defines the `Event` monad.

module Halogen.HTML.Events.Monad 
  ( Event(..)
  , unEvent
  
  , runEvent
  
  , yield
  , async
  , andThen
  ) where
      
import Data.Tuple
import Data.Maybe
import Data.Monoid

import Control.Apply ((*>))
import Control.Alt
import Control.Plus
import Control.Alternative
import Control.MonadPlus
import Control.Monad.Trans

import Control.Monad.ListT

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error())

import Control.Monad.Aff
import Control.Monad.Aff.Class
      
-- | The `Event` monad, which supports the asynchronous generation of events.
-- | 
-- | This monad is used in the definition of `runUI`.
newtype Event eff a = Event (ListT (Aff eff) a)

-- | Unwrap the `Event` constructor.
unEvent :: forall eff a. Event eff a -> ListT (Aff eff) a
unEvent (Event l) = l

-- | Run a computation in the `Event` monad by providing a callback function.
-- |
-- | The callback function will be invoked zero or more times.
runEvent :: forall eff a. (Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Event eff a -> Eff eff Unit
runEvent f s = go <<< unEvent
  where
  go :: ListT (Aff eff) a -> Eff eff Unit
  go l = runAff f handler (later (uncons l))
  
  handler :: Maybe (Tuple a (ListT (Aff eff) a)) -> Eff eff Unit
  handler Nothing = return unit
  handler (Just (Tuple a l)) = s a *> go l
  
-- | Yield an event. In practice, the event will be passed to the driver function.
yield :: forall eff a. a -> Event eff a
yield = async <<< pure

-- | Lift an asynchronous computation into the `Event` monad.
async :: forall eff a. Aff eff a -> Event eff a
async = liftAff

-- | A combinator which branches based on the supplied function after the first result,
-- | and returns to the original stream of events after the secondary stream has been
-- | exhausted.
andThen :: forall eff a. Event eff a -> (a -> Event eff a) -> Event eff a
andThen (Event l) f = Event (go l)
  where
  go :: ListT (Aff eff) a -> ListT (Aff eff) a
  go l = wrapEffect do
    m <- uncons l
    return case m of
      Nothing -> nil
      Just (Tuple a l1) -> singleton a <> unEvent (f a) <> go l1
  
instance semigroupEvent :: Semigroup (Event eff a) where
  (<>) (Event l1) (Event l2) = Event (l1 <> l2) 

instance monoidEvent :: Monoid (Event eff a) where
  mempty = Event mempty

instance functorEvent :: Functor (Event eff) where 
  (<$>) f (Event l) = Event (f <$> l)

instance applyEvent :: Apply (Event eff) where 
  (<*>) (Event f) (Event x) = Event (f <*> x)

instance applicativeEvent :: Applicative (Event eff) where
  pure = Event <<< pure

instance bindEvent :: Bind (Event eff) where
  (>>=) (Event l) f = Event (l >>= f >>> unEvent)

instance monadEvent :: Monad (Event eff)

instance monadEffEvent :: MonadEff eff (Event eff) where
  liftEff = Event <<< lift <<< liftEff

instance monadAffEvent :: MonadAff eff (Event eff) where
  liftAff = Event <<< lift

instance altEvent :: Alt (Event eff) where
  (<|>) (Event l1) (Event l2) = Event (l1 <|> l2)

instance plusEvent :: Plus (Event eff) where
  empty = Event empty

instance alternativeEvent :: Alternative (Event eff)

instance monadPlusEvent :: MonadPlus (Event eff)