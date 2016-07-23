module Halogen.RenderDSL where

import Prelude
import Data.Tuple (Tuple)
import Halogen.Component.Tree (Tree)

type SlotCreator h = forall a b. a -> h a b

class RenderDSL h where
  emptyTree :: forall f. Tree h f Unit
  initialTree :: forall f. h Void (f Unit) -> Tree h f Unit
  mapTree :: forall f f' p p'. f ~> f' -> (p -> p') -> Tree h f p -> Tree h f' p'
  installChildren :: forall st p t i i'.
      (SlotCreator h -> p -> st -> Tuple (h t i') st)
      -> (i -> i')
      -> h p i
      -> st
      -> Tuple (h t i') st
