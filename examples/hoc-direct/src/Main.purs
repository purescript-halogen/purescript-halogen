module Example.HocDirect.Main where

import Prelude

import Effect (Effect)
import Example.HocDirect.Button as Button
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Button.component unit body
