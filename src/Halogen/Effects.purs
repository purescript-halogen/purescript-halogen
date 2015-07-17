module Halogen.Effects where

import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Ref (REF())

import DOM (DOM())

type HalogenEffects eff =
  ( dom :: DOM
  , avar :: AVAR
  , console :: CONSOLE
  , err :: EXCEPTION
  | eff
  )
