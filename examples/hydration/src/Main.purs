module Example.Hydration.Main where

import Prelude

import Effect (Effect)
import Example.Hydration.App as App
import Halogen.Aff.Util as Halogen.Aff.Util
import Halogen.Aff as HA
import Halogen.VDom.Driver as Halogen.VDom.Driver
import Web.DOM.ParentNode as Web.DOM.ParentNode

main :: Effect Unit
main = HA.runHalogenAff do
  rootElement <- Halogen.Aff.Util.awaitElement (Web.DOM.ParentNode.QuerySelector "#root")
  Halogen.VDom.Driver.hydrateUI App.component unit rootElement
