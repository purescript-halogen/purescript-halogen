module Example.Effects.Eff.Random.Main where

import Prelude

import Effect (Effect)
import Example.Effects.Eff.Random.Component (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
