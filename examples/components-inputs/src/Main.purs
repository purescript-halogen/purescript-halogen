module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
