module Halogen
  ( HTML()
  , Prop()
  , module Halogen.Component
  , module Halogen.Driver
  , module Halogen.Effects
  , module Halogen.Query
  ) where

import Prelude
import Halogen.Component
import Halogen.Driver
import Halogen.Effects
import Halogen.Query
import qualified Halogen.HTML.Core as C

type HTML p i = C.HTML p (i Unit)
type Prop i = C.Prop (i Unit)

