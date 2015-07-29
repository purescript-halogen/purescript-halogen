-- | Convenience functions for working with form events.
module Halogen.HTML.Events.Forms
  ( onValueChange
  , onValueInput
  , onChecked
  ) where

import Prelude

import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (IsForeign, readProp)

import Halogen.HTML.Core (Prop(), eventName, handler)
import Halogen.HTML.Events.Handler (EventHandler())

-- | Attach event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall f value. (Applicative f, IsForeign value) => String -> String -> (value -> EventHandler (f Unit)) -> Prop (f Unit)
addForeignPropHandler key prop f = handler (eventName key) (either (const $ pure $ pure unit) f <<< readProp prop <<< toForeign <<< _.target)

-- | Attach an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall f. (Applicative f) => (String -> EventHandler (f Unit)) -> Prop (f Unit)
onValueChange = addForeignPropHandler "change" "value"

-- | Attach an event handler which will fire on input.
onValueInput :: forall f. (Applicative f) => (String -> EventHandler (f Unit)) -> Prop (f Unit)
onValueInput = addForeignPropHandler "input" "value"

-- | Attach an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall f. (Applicative f) => (Boolean -> EventHandler (f Unit)) -> Prop (f Unit)
onChecked = addForeignPropHandler "change" "checked"
