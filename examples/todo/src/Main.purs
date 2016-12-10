module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import Component.List (list)

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI list body
