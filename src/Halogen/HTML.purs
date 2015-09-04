module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Prelude ((<<<))
import Halogen.HTML.Core
import Halogen.HTML.Elements
import Halogen.Component.Inject (InjectC(), injPlaceholder)

text :: forall p i. String -> HTML p i
text = Text

placeholder :: forall p i. p -> HTML p i
placeholder = Placeholder

placeholder' :: forall s s' f f' p p' i. InjectC s s' f f' p p' -> p -> HTML p' i
placeholder' i = Placeholder <<< injPlaceholder i
