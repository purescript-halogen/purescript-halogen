-- | Helper functions for working with the `Aff` monad.

module Halogen.Mixin.Aff where
    
import DOM
    
import Data.Tuple
import Data.Either

import Control.Monad.Eff.Exception
import Control.Monad.Eff.Unsafe

import Control.Monad.Aff
    
import Halogen
import Halogen.HTML (HTML())
import Halogen.Signal
import Halogen.Internal.VirtualDOM
    
-- | This type class identifies those input types which support errors
class SupportsErrors input where
  liftError :: Error -> input

-- | This type synonym is provided to tidy up the signature of `runUIAff`.
type HandlerAff r i eff = r -> Aff (HalogenEffects eff) i

-- | A convenience function which uses the `Aff` monad to represent the handler function.
runUIAff :: forall i a r eff. (SupportsErrors i) => 
                              SF1 i (HTML a (Either i r)) ->
                              (a -> VTree) -> 
                              HandlerAff r i eff -> 
                              EffA (HalogenEffects eff) (Tuple Node (Driver i eff))
runUIAff signal renderComponent handler = unsafeInterleaveEff $ runUIEff signal renderComponent \r k -> unsafeInterleaveEff $ runAff (k <<< liftError) k $ handler r