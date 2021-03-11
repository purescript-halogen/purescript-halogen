module Halogen.Query.Event where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Halogen.Subscription as HS
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

-- | Constructs an `Emitter` for a DOM event. Accepts a function that maps event
-- | values to a `Maybe`-wrapped action, allowing it to filter events if
-- | necessary.
eventListener
  :: forall a
   . Event.EventType
  -> EventTarget.EventTarget
  -> (Event.Event -> Maybe a)
  -> HS.Emitter a
eventListener eventType target f =
  HS.makeEmitter \push -> do
    listener <- EventTarget.eventListener \ev -> traverse_ push (f ev)
    EventTarget.addEventListener eventType listener false target
    pure do
      EventTarget.removeEventListener eventType listener false target
