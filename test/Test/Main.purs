module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Component.ForkTest as ForkTest

main :: Effect Unit
main = launchAff_ do

  log "Test fork/kill"
  ForkTest.testForkKill

  log "Test fork killed on component finalize"
  ForkTest.testFinalize
