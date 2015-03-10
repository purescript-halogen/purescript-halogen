module Halogen.HTML.Attributes.Unsafe
  ( unsafeAttribute
  ) where

import Data.Tuple
import Data.Foreign (toForeign)
      
import Halogen.HTML
import Halogen.Internal.VirtualDOM
    
-- | This function can be used to define custom attributes.
unsafeAttribute :: forall i value. String -> value -> Attribute i
unsafeAttribute key value = Attribute [Tuple key (ValueAttribute (toForeign value))]
