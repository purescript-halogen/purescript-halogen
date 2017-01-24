module Test.DocExamples.Action where

import Prelude
import Control.Monad.Aff (Aff)
import Halogen (HalogenIO, action)
import Halogen.Aff (HalogenEffects)

data Query a = Tick a

sendTick :: forall o eff. HalogenIO Query o (Aff (HalogenEffects eff)) -> Aff (HalogenEffects eff) Unit
sendTick io = io.query (action Tick)
