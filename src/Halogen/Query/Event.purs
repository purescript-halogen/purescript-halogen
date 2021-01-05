module Halogen.Query.Event where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import FRP.Event as Event
import Web.Event.Event as E
import Web.Event.EventTarget as ET

-- | Constructs an `Event` for a DOM event. Accepts a function that maps event
-- | values to a `Maybe`-wrapped action, allowing it to filter events if
-- | necessary.
eventListenerEventSource
  :: forall a
   . E.EventType
  -> ET.EventTarget
  -> (E.Event -> Maybe a)
  -> Event.Event a
eventListenerEventSource eventType target f =
  Event.makeEvent \push -> do
    listener <- ET.eventListener \ev -> traverse_ push (f ev)
    ET.addEventListener eventType listener false target
    pure do
      ET.removeEventListener eventType listener false target
