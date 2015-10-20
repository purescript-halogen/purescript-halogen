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

import Prelude ((<<<), Unit(), unit, Functor, (<$>))

import Data.Bifunctor (bimap)

import Halogen.Component (Component(), SlotConstructor(..), transformChild)
import Halogen.Component.ChildPath (ChildPath(), injSlot, injState)
import Halogen.HTML.Core
import Halogen.HTML.Elements

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

-- | A convenience function around `slot`. Where `slot` allows you to define
-- | the function to call in order to initialize the component, `mkSlot` only
-- | lets you pass a component and initial state in.
-- | 
-- | ```purescript
-- | H.div_ [ H.mkSlot (TickSlot "A") ticker (TickState 100)
-- |        , H.mkSlot (TickSlot "B") ticker (TkcState 0)
-- |        ]
-- | ```
mkSlot :: forall s f g p i
        . p
       -> Component s f g
       -> s
       -> HTML (SlotConstructor s f g p) i
mkSlot p comp state = slot p \_ -> { component: comp, initialState: state }

-- | A convenience function around `slot` intended for use when mapping over
-- | a collection to make slots.
-- | 
-- | ```purescript
-- | map (H.indexedSlot TaskSlot task initialTask) state.taskList
-- | ```
indexedSlot :: forall s f g p i a
             . (a -> p)
            -> Component s f g
            -> s
            -> a
            -> HTML (SlotConstructor s f g p) i
indexedSlot p comp state i = mkSlot (p i) comp state

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
