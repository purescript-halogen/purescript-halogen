module Example.Effects.Aff.Ajax.Main where

import Prelude

import Effect (Effect)
import Example.Effects.Aff.Ajax.Component (component)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- | Run the app.
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
