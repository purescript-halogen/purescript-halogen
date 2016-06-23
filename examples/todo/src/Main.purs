module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)

import Component.List (list)
import Model (initialList)

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI list (H.parentState initialList) body
