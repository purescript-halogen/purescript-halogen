module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Component.ForkTest as ForkTest

main :: Effect Unit
main = launchAff_ do
  ForkTest.test
