module Test.DocExamples.Query.StateF.Get where

import Prelude
import Control.Monad.Free (Free())
import Halogen.Component (Eval())
import Halogen.Query.StateF (get)

data Input a = GetState (State -> a)
type State = Unit

eval :: forall g. (Functor g) => Eval Input State (Free Input) g
eval (GetState k) = do
  currentState <- get
  pure (k currentState)
