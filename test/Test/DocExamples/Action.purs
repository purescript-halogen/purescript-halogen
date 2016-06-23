module Test.DocExamples.Action where

import Prelude
import Halogen (HalogenEffects, Driver, action)
import Control.Monad.Aff (Aff)

data Query a = Tick a

sendTick :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Unit
sendTick driver = driver (action Tick)
