module Test.DocExamples.Action where

import Prelude
import Halogen
import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free())

data Query a = Tick a

sendTick :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Unit
sendTick driver = driver (action Tick)
