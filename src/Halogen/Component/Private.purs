module Halogen.Component.Private
  ( PrivateState()
  , PrivateQuery()
  , mkPrivate
  ) where

import Prelude
import Data.Functor.Coproduct (Coproduct())
import Unsafe.Coerce (unsafeCoerce)
import Halogen

data PrivateState
data PrivateQuery a

mkPrivate :: forall s s' f f' g p
           . Component (s s') (Coproduct f f') g
          -> p
          -> s s'
          -> SlotConstructor (s PrivateState) (Coproduct f PrivateQuery) g p
mkPrivate c p is =
  slotConstructor p \_ ->
    unsafeCoerce { component: c, initialState: is }
