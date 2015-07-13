module Halogen.Effects where

import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Ref (REF())

import DOM (DOM())

type HalogenEffects eff =
  ( dom :: DOM
  , ref :: REF
  , console :: CONSOLE
  , err :: EXCEPTION | eff
  )
