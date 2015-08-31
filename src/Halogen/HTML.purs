module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Halogen.HTML.Core
import Halogen.HTML.Elements

text :: forall p i. String -> HTML p i
text = Text

placeholder :: forall p i. p -> HTML p i
placeholder = Placeholder
