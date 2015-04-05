-- | This module defines the `EventHandler` functor, which can be used
-- | to perform standard operations on HTML events.

module Halogen.HTML.Events.Handler 
  ( EventHandler()
  , EventHandlerT()
  
  , preventDefault 
  , stopPropagation
  , stopImmediatePropagation
  , cancel
  
  , liftEventHandler
  
  , runEventHandler
  , unwrapEventHandler
  ) where

import DOM

import Data.Maybe
import Data.Tuple
import Data.Array ()
import Data.Foldable (for_)
import Data.Traversable (sequence)
import Data.Identity

import Control.Apply (lift2, (*>))
import Control.Plus (empty)
import Control.Monad.Eff

import Halogen.HTML.Events.Types

data EventUpdate 
  = PreventDefault
  | StopPropagation
  | StopImmediatePropagation

-- | This `Applicative` transformer supports the following operations on events:
-- |
-- | - `preventDefault`
-- | - `stopPropagation`
-- | - `stopImmediatePropagation`
-- | - `cancel`
-- |
-- | It can be used as follows:
-- |
-- | ```purescript
-- | import Control.Functor (($>))
-- |
-- | H.a (E.onclick \_ -> E.preventDefault $> ClickHandler) (H.text "Click here")
-- | ```
data EventHandlerT m a = EventHandlerT [EventUpdate] (Maybe (m a))

-- | `EventHandler` is a synonym for `EventHandlerT` applied to the `Identity` monad.
type EventHandler = EventHandlerT Identity
     
log :: forall m. (Applicative m) => EventUpdate -> EventHandlerT m Unit
log e = EventHandlerT [e] (Just (pure unit))

-- | Call the `preventDefault` method on the current event
preventDefault :: forall m. (Applicative m) => EventHandlerT m Unit
preventDefault = log PreventDefault
     
-- | Call the `stopPropagation` method on the current event
stopPropagation :: forall m. (Applicative m) => EventHandlerT m Unit
stopPropagation = log StopPropagation
     
-- | Call the `stopImmediatePropagation` method on the current event
stopImmediatePropagation :: forall m. (Applicative m) => EventHandlerT m Unit
stopImmediatePropagation = log StopImmediatePropagation
     
-- | Cancel the event, so that no input data will be passed to the signal function
cancel :: forall m a. EventHandlerT m a
cancel = EventHandlerT [] Nothing

-- | Lift an action into the `EventHandlerT` transformer
liftEventHandler :: forall m a. m a -> EventHandlerT m a
liftEventHandler = EventHandlerT [] <<< Just
     
-- | Interpret `EventHandlerT` in terms of `EventHandler`.
unwrapEventHandler :: forall m a. EventHandlerT m a -> EventHandler (m a)
unwrapEventHandler (EventHandlerT es ma) = EventHandlerT es (Identity <$> ma)
      
instance functorEventHandler :: (Functor m) => Functor (EventHandlerT m) where
  (<$>) f (EventHandlerT es m) = EventHandlerT es ((f <$>) <$> m)
  
instance applyEventHandler :: (Apply m) => Apply (EventHandlerT m) where
  (<*>) (EventHandlerT es1 m1) (EventHandlerT es2 m2) = EventHandlerT (es1 <> es2) (lift2 (<*>) m1 m2)

instance applicativeEventHandler :: (Applicative m) => Applicative (EventHandlerT m) where
  pure a = EventHandlerT [] (Just (pure a))
  
foreign import preventDefaultImpl
  "function preventDefaultImpl(e) {\
  \  return function() {\
  \    e.preventDefault();\
  \  };\
  \}" :: forall eff fields. Event fields -> Eff (dom :: DOM | eff) Unit
  
foreign import stopPropagationImpl
  "function stopPropagationImpl(e) {\
  \  return function() {\
  \    e.stopPropagation();\
  \  };\
  \}" :: forall eff fields. Event fields -> Eff (dom :: DOM | eff) Unit
  
foreign import stopImmediatePropagationImpl
  "function stopImmediatePropagationImpl(e) {\
  \  return function() {\
  \    e.stopImmediatePropagation();\
  \  };\
  \}" :: forall eff fields. Event fields -> Eff (dom :: DOM | eff) Unit
      
-- | This function can be used to update an event and return the wrapped value
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) (Maybe a)
runEventHandler e (EventHandlerT es ma) = do
  for_ es applyUpdate
  return (runIdentity (sequence ma))
  where
  applyUpdate :: EventUpdate -> Eff (dom :: DOM | eff) Unit
  applyUpdate PreventDefault            = preventDefaultImpl e
  applyUpdate StopPropagation           = stopPropagationImpl e
  applyUpdate StopImmediatePropagation  = stopImmediatePropagationImpl e