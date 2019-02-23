module Example.HOC.Main where

import Prelude

import Effect (Effect)
import Example.HOC.Harness as Harness
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Harness.component unit body
