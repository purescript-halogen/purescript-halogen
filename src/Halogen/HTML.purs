-- | This module re-exports the core types for the `HTML` DSL, and values for
-- | all supported HTML elements.
-- |
-- | Consider using the `Halogen.HTML.Indexed` variety of this module for
-- | better type safety.
module Halogen.HTML
  ( module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  ) where

import Prelude (Unit(), class Functor, (<$>))

import Halogen.Component (Component(), SlotConstructor(..), transformChild)
import Halogen.Component.ChildPath (ChildPath(), injSlot, injState)
import Halogen.HTML.Core (..)
import Halogen.HTML.Elements (..)

-- | Constructs a text node `HTML` value.
text :: forall p i. String -> HTML p i
text = Text

-- | Defines a slot for a child component. Takes a slot "address" value and a
-- | thunked constructor.
slot :: forall s f g p i
      . p
     -> (Unit -> { component :: Component s f g, initialState :: s })
     -> HTML (SlotConstructor s f g p) i
slot p l = Slot (SlotConstructor p l)

-- | Defines a slot for a child component when a parent has multiple types of
-- | child component. Takes the `ChildPath` for the child component's type, a
-- | slot "address" value and a thunked constructor.
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
