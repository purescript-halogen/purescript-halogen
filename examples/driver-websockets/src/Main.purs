module Example.Driver.Websockets.Main where

import Prelude

import Effect (Effect)
import Example.Driver.Websockets.Log as Log
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Log.component unit body
