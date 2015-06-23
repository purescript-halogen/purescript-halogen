module Test.DocExamples.Action where

import Prelude
import Halogen
import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free())

data Input a = Tick a

sendTick :: forall eff. Driver Input eff -> Aff (HalogenEffects eff) Unit
sendTick driver = driver (action Tick)
