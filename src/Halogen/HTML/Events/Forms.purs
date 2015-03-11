-- | Convenience functions for working with form elements.

module Halogen.HTML.Events.Forms 
  ( onValueChanged
  , onChecked
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
  
-- | Attach an event handler which will produce an input when the value of an input field changes
-- |
-- | An input will not be produced if the value cannot be cast to the appropriate type.
onValueChanged :: forall value i. (IsForeign value) => (value -> EventHandler i) -> H.Attribute i
onValueChanged f = unsafeHandler' (H.attributeName "change") \e -> traverse f (readValue e.target)
  where
  readValue :: Node -> Maybe value
  readValue = either (const Nothing) Just <<< readProp "value" <<< toForeign
  
-- | Attach an event handler which will fire when a checkbox is checked or unchecked
onChecked :: forall i. (Boolean -> EventHandler i) -> H.Attribute i
onChecked f = unsafeHandler' (H.attributeName "change") \e -> traverse f (readChecked e.target)
  where
  readChecked :: Node -> Maybe Boolean
  readChecked = either (const Nothing) Just <<< readProp "checked" <<< toForeign