module Test.ReadmeExamples.Input where

import Prelude
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

data ExampleQuery a
  = SetOption Boolean a
  | SetText String a

input1 :: P.IProp (checked :: P.I, onChange :: P.I) (ExampleQuery Unit)
input1 = E.onChecked (E.input SetOption)

input2 :: P.IProp (value :: P.I, onInput :: P.I) (ExampleQuery Unit)
input2 = E.onValueInput (E.input SetText)
