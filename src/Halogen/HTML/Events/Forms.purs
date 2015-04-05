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

import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Attributes as H
  
-- | Attach event handler to event ```key``` with getting ```prop``` field
-- | as an argument of handler
addForeignPropHandler :: forall value m i. (IsForeign value) => String -> String -> (value -> EventHandlerT m i) -> H.Attr (m i)
addForeignPropHandler key prop f = E.handlerT (H.eventName key) (\e -> handler (toForeign e.target))
  where
  handler :: Foreign -> EventHandlerT m i
  handler e = case readProp prop e of
                Left _ -> cancel
                Right i -> f i

-- | Attach an event handler which will produce an input when the value of an input field changes
-- |
-- | An input will not be produced if the value cannot be cast to the appropriate type.
onValueChanged :: forall value m i. (IsForeign value) => (value -> EventHandlerT m i) -> H.Attr (m i)
onValueChanged = addForeignPropHandler "change" "value"

-- | Attach an event handler which will fire when a checkbox is checked or unchecked
onChecked :: forall m i. (Boolean -> EventHandlerT m i) -> H.Attr (m i)
onChecked = addForeignPropHandler "change" "checked"

-- | Attach an event handler which will fire on input
onInput :: forall value m i. (IsForeign value) => (value -> EventHandlerT m i) -> H.Attr (m i)
onInput = addForeignPropHandler "input" "value"
