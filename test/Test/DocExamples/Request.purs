module Test.DocExamples.Request where

import Prelude
import Halogen
import Control.Monad.Aff (Aff)

data Query a = GetTickCount (Int -> a)

getTickCount :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Int
getTickCount driver = driver (request GetTickCount)
