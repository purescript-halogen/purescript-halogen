module Test.DocExamples.Action where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)
import Halogen as H

data Query a = Tick a

sendTick :: forall o. H.HalogenIO Query o Aff -> Aff (Maybe Unit)
sendTick io = io.query (H.mkTell Tick)
