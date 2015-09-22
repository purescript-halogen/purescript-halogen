module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Prelude ((<<<))
import Halogen.HTML.Core
import Halogen.HTML.Elements
import Halogen.Component.Inject (InjectC(), injSlot)

text :: forall p i. String -> HTML p i
text = Text

slot :: forall p i. p -> HTML p i
slot = Slot

slot' :: forall s s' f f' p p' i. InjectC s s' f f' p p' -> p -> HTML p' i
slot' i = Slot <<< injSlot i
