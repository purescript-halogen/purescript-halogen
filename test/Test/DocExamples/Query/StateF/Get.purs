module Test.DocExamples.Query.StateF.Get where

import Prelude
import Halogen.Component (ComponentDSL)
import Halogen.Query (get)

data Query a = GetState (State -> a)
type State = Unit

eval :: forall g. Query ~> ComponentDSL State Query g
eval (GetState k) = do
  currentState <- get
  pure (k currentState)
