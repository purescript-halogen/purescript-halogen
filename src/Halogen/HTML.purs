module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Prelude ((<<<), Unit(), unit, Functor)
import Data.Bifunctor (bimap)
import Halogen.HTML.Core
import Halogen.HTML.Elements
import Halogen.Component (Component(), SlotConstructor(..), transformChild)
import Halogen.Component.ChildPath (ChildPath(), injSlot, injState)

text :: forall p i. String -> HTML p i
text = Text

slot :: forall s f g p i
      . p
     -> Component s f g
     -> (Unit -> s)
     -> HTML (SlotConstructor s f g p) i
slot p c s = Slot (SlotConstructor p c s)

slot' :: forall s s' f f' g p p' i
       . (Functor g)
      => ChildPath s s' f f' p p'
      -> p
      -> Component s f g
      -> (Unit -> s)
      -> HTML (SlotConstructor s' f' g p') i
slot' i p c s = Slot (SlotConstructor (injSlot i p) (transformChild i c) (\_ -> injState i (s unit)))
