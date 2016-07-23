module Halogen.Component.Tree
  ( Tree
  , TreeF
  , mkTree
  , mkTree'
  , runTree
  , graftTree
  , thunkTree
  ) where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Lazy (Lazy)
import Unsafe.Coerce (unsafeCoerce)

type TreeF h f p p' =
  { slot :: p
  , html :: Lazy (h (Tree h f p') (f Unit))
  , eq :: p' -> p' -> Boolean
  , thunk :: Boolean
  }

foreign import data Tree :: (* -> * -> *) -> (* -> *) -> * -> *

mkTree :: forall h f p'. Eq p' => Lazy (h (Tree h f p') (f Unit)) -> Tree h f Unit
mkTree html =
  mkTree'
    { slot: unit
    , html: html
    , eq: eq
    , thunk: false
    }

mkTree' :: forall h f p p'. TreeF h f p p' -> Tree h f p
mkTree' = unsafeCoerce

runTree :: forall h f p r. (forall p'. TreeF h f p p' -> r) -> Tree h f p -> r
runTree k t = case unsafeCoerce t of tree -> k tree

graftTree :: forall h f f' p p'. (Bifunctor h) => f ~> f' -> (p -> p') -> Tree h f p -> Tree h f' p'
graftTree l r = runTree \t ->
  mkTree'
    { slot: r t.slot
    , html: bimap (graftTree l id) l <$> t.html
    , eq: t.eq
    , thunk: t.thunk
    }

thunkTree :: forall h f p. Tree h f p -> Tree h f p
thunkTree = runTree (mkTree' <<< _ { thunk = true })
