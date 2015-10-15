module Test.DocExamples.Query.StateF.Gets where

import Prelude
import Control.Monad.Free (Free())
import Halogen.Component (Eval())
import Halogen.Query (gets)

data Query a = GetX (Number -> a)
type State = { x :: Number, y :: Number }

eval :: forall g. Eval Query State Query g
eval (GetX k) = do
  x <- gets _.x
  pure (k x)
