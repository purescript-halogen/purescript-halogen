module Test.ReadmeExamples.Handler where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen (action)
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

data Query a = ToggleState a

prevented :: P.IProp (onClick :: P.I) (Query Unit)
prevented =
  E.onClick (\_ -> EH.preventDefault $> Just (action ToggleState))

preventedNoPropagate :: P.IProp (onClick :: P.I) (Query Unit)
preventedNoPropagate =
  E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> Just (action ToggleState))
