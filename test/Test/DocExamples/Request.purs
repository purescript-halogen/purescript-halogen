module Test.DocExamples.Request where

import Halogen
import Control.Monad.Aff (Aff)
import Halogen.Aff (HalogenEffects)

data Query a = GetTickCount (Int -> a)

getTickCount :: forall o eff. HalogenIO Query o (Aff (HalogenEffects eff)) -> Aff (HalogenEffects eff) Int
getTickCount app = app.query (request GetTickCount)
