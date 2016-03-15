module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Plus (Plus)

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)

import Model
import Component.List

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI list (parentState initialList) body
