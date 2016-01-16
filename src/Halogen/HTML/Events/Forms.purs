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
import Data.Maybe (Maybe(..))

import Halogen.HTML.Core (Prop(), eventName, handler')
import Halogen.HTML.Events.Handler (EventHandler())

-- | Attaches event handler to event `key` with getting `prop` field as an
-- | argument of `handler`.
addForeignPropHandler :: forall i value. (IsForeign value) => String -> String -> (value -> EventHandler i) -> Prop i
addForeignPropHandler key prop f = handler' (eventName key) (either (const $ pure Nothing) (map Just <<< f) <<< readProp prop <<< toForeign <<< _.target)

-- | Attaches an event handler which will produce an input when the value of an
-- | input field changes.
onValueChange :: forall i. (String -> EventHandler i) -> Prop i
onValueChange = addForeignPropHandler "change" "value"

-- | Attaches an event handler which will fire on input.
onValueInput :: forall i. (String -> EventHandler i) -> Prop i
onValueInput = addForeignPropHandler "input" "value"

-- | Attaches an event handler which will fire when a checkbox is checked or
-- | unchecked.
onChecked :: forall i. (Boolean -> EventHandler i) -> Prop i
onChecked = addForeignPropHandler "change" "checked"
