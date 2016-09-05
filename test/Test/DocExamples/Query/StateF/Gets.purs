module Test.DocExamples.Query.StateF.Gets where

import Prelude
import Halogen.Component (ComponentDSL)
import Halogen.Query (gets)

data Query a = GetX (Number -> a)
type State = { x :: Number, y :: Number }

eval :: forall m. Query ~> ComponentDSL State Query Void m
eval (GetX k) = do
  x <- gets _.x
  pure (k x)
