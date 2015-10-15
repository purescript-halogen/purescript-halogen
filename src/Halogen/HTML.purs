module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Prelude ((<<<), Unit(), unit, Functor, (<$>))

import Data.Bifunctor (bimap)

import Halogen.Component (Component(), SlotConstructor(..), transformChild)
import Halogen.Component.ChildPath (ChildPath(), injSlot, injState)
import Halogen.HTML.Core
import Halogen.HTML.Elements

text :: forall p i. String -> HTML p i
text = Text

slot :: forall s f g p i
      . p
     -> (Unit -> { component :: Component s f g, initialState :: s })
     -> HTML (SlotConstructor s f g p) i
slot p l = Slot (SlotConstructor p l)

slot' :: forall s s' f f' g p p' i
       . (Functor g)
      => ChildPath s s' f f' p p'
      -> p
      -> (Unit -> { component :: Component s f g, initialState :: s })
      -> HTML (SlotConstructor s' f' g p') i
slot' i p l = Slot (SlotConstructor (injSlot i p) (transform <$> l))
  where
  transform def =
    { component: transformChild i def.component
    , initialState: injState i def.initialState
    }
