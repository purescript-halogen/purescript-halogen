module Halogen.HTML.Events.Unsafe
  ( unsafeHandler
  , unsafeHandler'
  ) where
      
import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
      
import Halogen.HTML
import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler
import Halogen.Internal.VirtualDOM
    
-- | This function can be used to attach custom event handlers.
unsafeHandler :: forall fields i. String -> (Event fields -> EventHandler i) -> Attribute i
unsafeHandler key f = unsafeHandler' key \e -> Just <$> f e

-- | This function can be used to attach custom event handlers.
unsafeHandler' :: forall fields eff i. String -> (Event fields -> EventHandler (Maybe i)) -> Attribute i
unsafeHandler' key f = attribute \k props -> runFn3 handlerProp key (\e -> unsafeInterleaveEff (runEventHandler e (f e)) >>= maybe (return unit) k) props