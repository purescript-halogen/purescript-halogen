module Test.DocExamples.Query.StateF.Modify where

import Prelude
import Data.NaturalTransformation (Natural)
import Halogen.Component (ComponentDSL)
import Halogen.Query (modify)

data Query a = Increment a
type State = Int

eval :: forall g. Natural Query (ComponentDSL State Query g)
eval (Increment next) = do
  modify (+ 1)
  pure next
