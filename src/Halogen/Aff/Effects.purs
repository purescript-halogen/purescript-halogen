module Halogen.Aff.Effects where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)

-- | A type alias for the basic row of effects involved in running Halogen with
-- | `Aff`-based drivers.
type HalogenEffects eff =
  ( avar :: AVAR
  , ref :: REF
  , exception :: EXCEPTION
  , dom :: DOM
  | eff
  )
