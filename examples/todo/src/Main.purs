module Example.Todo.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Example.Todo.Component.List (list)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI list unit body
