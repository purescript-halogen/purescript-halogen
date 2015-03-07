module Halogen.HTML.Forms 
  ( onValueStringChanged
  
  ) where
    
import DOM

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Halogen.HTML.Attributes
  
-- | Attach an event handler which will produce an input when the value of an input field changes
-- |
-- | An input will not be produced if the value cannot be cast to the appropriate type.
onValueStringChanged :: forall value i. (IsForeign value) => (value -> i) -> Attribute i
onValueStringChanged f = unsafeHandler' "onchange" \e -> f <$> readValue e.target
  where
  readValue :: Node -> Maybe value
  readValue = either (const Nothing) Just <<< readProp "value" <<< toForeign
  
-- | Attach an event handler which will fire when a checkbox is checked or unchecked
onChecked :: forall i. (Boolean -> i) -> Attribute i
onChecked f = unsafeHandler' "onchange" \e -> f <$> readChecked e.target
  where
  readChecked :: Node -> Maybe Boolean
  readChecked = either (const Nothing) Just <<< readProp "checked" <<< toForeign