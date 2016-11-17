module Test.DocExamples.Request where

import Halogen
import Halogen.VirtualDOM.Driver (HalogenIO)
import Control.Monad.Aff (Aff)

data Query a = GetTickCount (Int -> a)

getTickCount :: forall o eff. HalogenIO Query o (Aff (HalogenEffects eff)) -> Aff (HalogenEffects eff) Int
getTickCount app = app.query (request GetTickCount)
