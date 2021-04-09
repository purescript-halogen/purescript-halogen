module Example.MemoizedLazy.Main where

import Prelude

import Effect (Effect)
import Example.MemoizedLazy.Container as Container
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Container.component unit body
