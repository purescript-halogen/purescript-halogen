module Test.DocExamples.Request where

import Halogen
import Effect.Aff (Aff)

data Query a = GetTickCount (Int -> a)

getTickCount :: forall o. HalogenIO Query o Aff -> Aff Int
getTickCount app = app.query (request GetTickCount)
