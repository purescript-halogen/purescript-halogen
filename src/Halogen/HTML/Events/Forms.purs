-- | Convenience functions for working with form elements.

module Halogen.HTML.Events.Forms 
  ( onValueChanged
  , onChecked
  , onInput
  ) where
    
import DOM

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.Traversable (traverse)

import Halogen.HTML.Events.Handler
import Halogen.HTML.Events.Unsafe (unsafeHandler')

import qualified Halogen.HTML as H
  
-- | Attach event handler to event ```key``` with getting ```prop``` field
-- | as an argument of handler
addForeignPropHandler :: forall i value. (IsForeign value) =>
               String -> String -> (value -> EventHandler i) -> H.Attribute i
addForeignPropHandler key prop f = unsafeHandler' (H.attributeName key)
                         \e -> traverse f (getProp prop e.target)
  where
    getProp :: String -> Node -> Maybe value
    getProp prop = either (const Nothing) Just <<< readProp prop <<< toForeign

-- | Attach an event handler which will produce an input when the value of an input field changes
-- |
-- | An input will not be produced if the value cannot be cast to the appropriate type.
onValueChanged :: forall value i. (IsForeign value) =>
                  (value -> EventHandler i) -> H.Attribute i
onValueChanged = addForeignPropHandler "change" "value"

-- | Attach an event handler which will fire when a checkbox is checked or unchecked
onChecked :: forall i. (Boolean -> EventHandler i) -> H.Attribute i
onChecked = addForeignPropHandler "change" "checked"

-- | Attach an event handler which will fire on input
onInput :: forall value i. (IsForeign value) =>
           (value -> EventHandler i) -> H.Attribute i
onInput = addForeignPropHandler "input" "value"
