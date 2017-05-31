module Main where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import Component (ui)

-- | Run the app.
main :: Eff (HA.HalogenEffects (dom :: DOM, ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
