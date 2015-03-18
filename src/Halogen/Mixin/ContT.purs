-- | Helper functions for working with the `ContT` monad.

module Halogen.Mixin.ContT where
    
import DOM
    
import Data.Tuple
import Data.Either

import Control.Monad.Eff

import Control.Monad.Cont.Trans
    
import Halogen
import Halogen.HTML (HTML())
import Halogen.Signal
import Halogen.Internal.VirtualDOM

-- | This type synonym is provided to tidy up the signature of `runUICont`.
type HandlerCont r i eff = r -> ContT Unit (Eff (HalogenEffects eff)) i

-- | A convenience function which uses the `ContT` monad to represent the handler function.
runUICont :: forall i a r eff. SF1 i (HTML a (Either i r)) ->
                               (a -> VTree) -> 
                               HandlerCont r i eff -> 
                               Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
runUICont signal renderComponent handler = runUIEff signal renderComponent (runContT <<< handler)