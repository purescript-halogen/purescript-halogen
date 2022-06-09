module Example.RawHTML.Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen as H

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> unit
  , render: \_ -> template
  , eval: H.mkEval $ H.defaultEval { handleAction = \_ -> pure unit }
  }

template :: forall action m. H.ComponentHTML action (()) m
template = HH.rawHTML "<b>hello world</b>"
