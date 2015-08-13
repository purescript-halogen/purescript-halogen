module Test.DocExamples.Query.StateF.Modify where

import Prelude
import Control.Monad.Free (Free())
import Halogen.Component (Eval())
import Halogen.Query.StateF (modify)

data Input a = Increment a
type State = Int

eval :: forall g. (Functor g) => Eval Input State (Free Input) g
eval (Increment next) = do
  modify (+ 1)
  pure next
