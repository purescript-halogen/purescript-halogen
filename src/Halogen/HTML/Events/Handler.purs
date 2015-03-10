module Halogen.HTML.Events.Handler 
  ( EventHandler()
  
  , preventDefault 
  , stopPropagation
  , stopImmediatePropagation
  
  , runEventHandler
  ) where

import DOM

import Data.Foldable (for_)

import Control.Apply ((*>))
import Control.Monad.Eff

import Halogen.HTML.Events.Types

data EventUpdate 
  = PreventDefault
  | StopPropagation
  | StopImmediatePropagation

-- | This applicative functor supports the following operations on events:
-- |
-- | - `preventDefault`
-- | - `stopPropagation`
-- | - `stopImmediatePropagation`
-- |
-- | It can be used as follows:
-- |
-- | ```purescript
-- | import Control.Functor (($>))
-- |
-- | H.a (E.onclick \_ -> E.preventDefault $> ClickHandler) (H.text "Click here")
-- | ```
data EventHandler a = EventHandler [EventUpdate] a
     
-- | Call the `preventDefault` method on the current event
preventDefault :: EventHandler Unit
preventDefault = EventHandler [PreventDefault] unit
     
-- | Call the `stopPropagation` method on the current event
stopPropagation :: EventHandler Unit
stopPropagation = EventHandler [StopPropagation] unit
     
-- | Call the `stopImmediatePropagation` method on the current event
stopImmediatePropagation :: EventHandler Unit
stopImmediatePropagation = EventHandler [StopImmediatePropagation] unit
      
instance functorEventHandler :: Functor EventHandler where
  (<$>) f (EventHandler us a) = EventHandler us (f a)

instance applyEventHandler :: Apply EventHandler where
  (<*>) (EventHandler us1 f) (EventHandler us2 x) = EventHandler (us1 <> us2) (f x)

instance applicativeEventHandler :: Applicative EventHandler where
  pure = EventHandler []
  
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
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) a
runEventHandler e (EventHandler us a) = for_ us applyUpdate *> return a
  where
  applyUpdate :: EventUpdate -> Eff (dom :: DOM | eff) Unit
  applyUpdate PreventDefault            = preventDefaultImpl e
  applyUpdate StopPropagation           = stopPropagationImpl e
  applyUpdate StopImmediatePropagation  = stopImmediatePropagationImpl e