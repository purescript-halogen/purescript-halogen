module Test.DocExamples.Action where

import Prelude
import Effect.Aff (Aff)
import Halogen (HalogenIO, action)

data Query a = Tick a

sendTick :: forall o. HalogenIO Query o Aff -> Aff Unit
sendTick io = io.query (action Tick)
