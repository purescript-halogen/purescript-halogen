module Test.DocExamples.Query.StateF.Get where

import Prelude
import Control.Monad.Free (Free())
import Halogen.Component (Eval())
import Halogen.Query (get)

data Query a = GetState (State -> a)
type State = Unit

eval :: forall g. Eval Query State Query g
eval (GetState k) = do
  currentState <- get
  pure (k currentState)
