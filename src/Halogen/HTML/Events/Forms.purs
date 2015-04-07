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

import Control.Plus
import Control.Alternative

import Halogen.HTML.Events.Handler

import qualified Halogen.HTML.Attributes as H
  
-- | Attach event handler to event ```key``` with getting ```prop``` field
-- | as an argument of handler
addForeignPropHandler :: forall f i value. (Alternative f, IsForeign value) => String -> String -> (value -> EventHandler (f i)) -> H.Attr (f i)
addForeignPropHandler key prop f = H.handler (H.eventName key) (\e -> handler (toForeign e.target))
  where
  handler :: Foreign -> EventHandler (f i)
  handler e = case readProp prop e of
                Left _ -> pure empty
                Right i -> f i

-- | Attach an event handler which will produce an input when the value of an input field changes
-- |
-- | An input will not be produced if the value cannot be cast to the appropriate type.
onValueChanged :: forall value f i. (Alternative f, IsForeign value) => (value -> EventHandler (f i)) -> H.Attr (f i)
onValueChanged = addForeignPropHandler "change" "value"

-- | Attach an event handler which will fire when a checkbox is checked or unchecked
onChecked :: forall f i. (Alternative f) => (Boolean -> EventHandler (f i)) -> H.Attr (f i)
onChecked = addForeignPropHandler "change" "checked"

-- | Attach an event handler which will fire on input
onInput :: forall f value i. (Alternative f, IsForeign value) => (value -> EventHandler (f i)) -> H.Attr (f i)
onInput = addForeignPropHandler "input" "value"
