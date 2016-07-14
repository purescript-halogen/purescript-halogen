module Halogen.Component.Slot where

import Data.Lazy (Lazy)
import Halogen.Component.Types (Component)

data ComponentSlot f' g p = ComponentSlot p (Lazy (Component f' g))
