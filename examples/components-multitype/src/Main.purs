module Example.Components.Multitype.Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Example.Components.Multitype.Container as Container

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
