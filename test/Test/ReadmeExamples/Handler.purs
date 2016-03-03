module Test.ReadmeExamples.Handler where

import Prelude
import Halogen
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Data.Functor (($>))
import Halogen.HTML.Events.Handler as EH

data Query a = ToggleState a

handled :: P.IProp (onClick :: P.I) (Query Unit)
handled = E.onClick (\_ -> EH.preventDefault $> action ToggleState)
