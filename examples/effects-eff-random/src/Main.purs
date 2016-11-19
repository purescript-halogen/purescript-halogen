module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.VirtualDOM.Driver (runUI)
import Control.Monad.Eff.Random (RANDOM)
import Component (ui)

main :: Eff (H.HalogenEffects (random :: RANDOM)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
