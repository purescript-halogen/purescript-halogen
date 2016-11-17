module Test.DocExamples.Action where

import Prelude
import Halogen (HalogenEffects, action)
import Halogen.VirtualDOM.Driver (HalogenIO)
import Control.Monad.Aff (Aff)

data Query a = Tick a

sendTick :: forall o eff. HalogenIO Query o (Aff (HalogenEffects eff)) -> Aff (HalogenEffects eff) Unit
sendTick app = app.query (action Tick)
