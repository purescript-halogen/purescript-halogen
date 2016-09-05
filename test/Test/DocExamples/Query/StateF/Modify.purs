module Test.DocExamples.Query.StateF.Modify where

import Prelude
import Halogen.Component (ComponentDSL)
import Halogen.Query (modify)

data Query a = Increment a
type State = Int

eval :: forall m. Query ~> ComponentDSL State Query Void m
eval (Increment next) = do
  modify (_ + 1)
  pure next
