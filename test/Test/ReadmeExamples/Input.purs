module Test.ReadmeExamples.Input where

import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

data ExampleQuery a
  = SetOption Boolean a
  | SetText String a

input1 :: forall p. P.IProp (checked :: P.I, onChange :: P.I) ExampleQuery p
input1 = E.onChecked (E.input SetOption)

input2 :: forall p. P.IProp (value :: P.I, onInput :: P.I) ExampleQuery p
input2 = E.onValueInput (E.input SetText)
