module Test.DocExamples.Query.StateF.Modify where

import Prelude
import Control.Monad.Free (Free())
import Halogen.Component (Eval())
import Halogen.Query (modify)

data Query a = Increment a
type State = Int

eval :: forall g. Eval Query State Query g
eval (Increment next) = do
  modify (+ 1)
  pure next
