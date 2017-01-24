module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Container as Container

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
