module Halogen.HTML.Attributes.Unsafe
  ( unsafeAttribute
  , unsafeHandler
  , unsafeHandler'
  ) where
      
import Data.Maybe
import Data.Function
      
import Halogen.HTML
import Halogen.Internal.VirtualDOM
    
-- | This function can be used to define custom attributes.
unsafeAttribute :: forall i value. String -> value -> Attribute i
unsafeAttribute key value = attribute \_ props -> runFn3 prop key value props

-- | This function can be used to attach custom event handlers.
unsafeHandler :: forall event eff i. String -> (event -> i) -> Attribute i
unsafeHandler key f = unsafeHandler' key (Just <<< f)

-- | This function can be used to attach custom event handlers.
unsafeHandler' :: forall event i. String -> (event -> Maybe i) -> Attribute i
unsafeHandler' key f = attribute \k props -> runFn3 handlerProp key (\e -> maybe (return unit) k (f e)) props