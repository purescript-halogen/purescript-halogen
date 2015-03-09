module Halogen.HTML.Attributes.Unsafe
  ( unsafeAttribute
  ) where

import Data.Function
      
import Halogen.HTML
import Halogen.Internal.VirtualDOM
    
-- | This function can be used to define custom attributes.
unsafeAttribute :: forall i value. String -> value -> Attribute i
unsafeAttribute key value = attribute \_ props -> runFn3 prop key value props
