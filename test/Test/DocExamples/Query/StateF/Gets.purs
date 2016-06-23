module Test.DocExamples.Query.StateF.Gets where

import Prelude
import Halogen.Component (ComponentDSL)
import Halogen.Query (gets)

data Query a = GetX (Number -> a)
type State = { x :: Number, y :: Number }

eval :: forall g. Query ~> ComponentDSL State Query g
eval (GetX k) = do
  x <- gets _.x
  pure (k x)
