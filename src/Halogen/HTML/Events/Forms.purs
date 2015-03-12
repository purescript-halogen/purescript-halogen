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
  
-- Attach event handler to event ```key``` with getting ```prop``` field
-- as an argument of handler
onSomething :: forall i value. (IsForeign value) =>
               String -> String -> (value -> EventHandler i) -> H.Attribute i
onSomething key prop f = unsafeHandler' (H.attributeName key)
                         \e -> traverse f (getProp prop e.target)
  where
    getProp :: String -> Node -> Maybe value
    getProp prop = either (const Nothing) Just <<< readProp prop <<< toForeign

onValueChanged :: forall value i. (IsForeign value) =>
                  (value -> EventHandler i) -> H.Attribute i
onValueChanged = onSomething "change" "value"

onChecked :: forall i. (Boolean -> EventHandler i) -> H.Attribute i
onChecked = onSomething "change" "checked"

onInput :: forall value i. (IsForeign value) =>
           (value -> EventHandler i) -> H.Attribute i
onInput = onSomething "input" "value"
