module Example.Todo.Main where

import Prelude
import Effect (Effect)
import Example.Todo.Component.List (list)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI list unit body
