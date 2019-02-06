module Example.Profunctor.Main where

import Prelude

import Effect (Effect)
import Example.Profunctor.App (app)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app unit body
