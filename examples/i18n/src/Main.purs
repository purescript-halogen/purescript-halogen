module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Foldable (traverse_)
import Data.Reflection (reify)
import DOM.Node.ParentNode (QuerySelector(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Container as Container
import I18n as I18n

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  _ <- HA.awaitBody

  HA.selectElement (QuerySelector "#ui1")
    >>= traverse_ (runUI (reify I18n.english Container.component) unit)

  HA.selectElement (QuerySelector "#ui2")
    >>= traverse_ (runUI (reify I18n.french Container.component) unit)
