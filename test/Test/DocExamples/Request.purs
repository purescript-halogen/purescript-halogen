module Test.DocExamples.Request where

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H

data Query a = GetTickCount (Int -> a)

getTickCount :: forall o. H.HalogenIO Query o Aff -> Aff (Maybe Int)
getTickCount app = app.query (H.mkRequest GetTickCount)
