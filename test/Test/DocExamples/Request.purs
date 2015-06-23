module Test.DocExamples.Request where

import Prelude
import Halogen
import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free())

data Input a = GetTickCount (Int -> a)

getTickCount :: forall eff. Driver Input eff -> Aff (HalogenEffects eff) Int
getTickCount driver = driver (request GetTickCount)
