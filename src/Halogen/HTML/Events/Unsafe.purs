-- | Unsafe functions for working with DOM events

module Halogen.HTML.Events.Unsafe
  ( unsafeHandler
  , unsafeHandler'
  ) where
      
import Data.Maybe
import Data.Tuple
import Data.Foreign (unsafeFromForeign)

import Control.Monad.Eff
      
import Halogen.HTML
import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler
import Halogen.Internal.VirtualDOM
    
-- | This function can be used to attach custom event handlers.
unsafeHandler :: forall fields i. String -> (Event fields -> EventHandler i) -> Attribute i
unsafeHandler key f = unsafeHandler' key \e -> Just <$> f e

-- | This function can be used to attach custom event handlers.
unsafeHandler' :: forall fields eff i. String -> (Event fields -> EventHandler (Maybe i)) -> Attribute i
unsafeHandler' key f = Attribute [Tuple key (HandlerAttribute \e -> f (unsafeFromForeign e))]