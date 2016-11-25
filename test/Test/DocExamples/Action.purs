module Test.DocExamples.Action where

import Prelude
import Control.Monad.Aff (Aff)
import Halogen (action)
import Halogen.Aff (HalogenEffects)
import Halogen.VirtualDOM.Driver (HalogenIO)

data Query a = Tick a

sendTick :: forall o eff. HalogenIO Query o (Aff (HalogenEffects eff)) -> Aff (HalogenEffects eff) Unit
sendTick io = io.query (action Tick)
