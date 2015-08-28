module Test.DocExamples.Query.StateF.Gets where

import Prelude
import Control.Monad.Free (Free())
import Halogen.Component (Eval())
import Halogen.Query (gets)

data Input a = GetX (Number -> a)
newtype State = State { x :: Number, y :: Number }

eval :: forall g. (Functor g) => Eval Input State (Free Input) g
eval (GetX k) = do
  x <- gets \(State st) -> st.x
  pure (k x)
