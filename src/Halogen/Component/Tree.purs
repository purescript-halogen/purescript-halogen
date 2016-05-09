module Halogen.Component.Tree
  ( Tree()
  , TreeF()
  , mkTree
  , mkTree'
  , runTree
  , graftTree
  , thunkTree
  , emptyTree
  ) where

import Prelude
import Data.Bifunctor (bimap)
import Data.Lazy (Lazy(), defer)
import Data.NaturalTransformation (Natural())
import Halogen.HTML.Core (HTML(..))
import Unsafe.Coerce (unsafeCoerce)

type TreeF f p p' =
  { slot :: p
  , html :: Lazy (HTML (Tree f p') (f Unit))
  , eq :: p' -> p' -> Boolean
  , thunk :: Boolean
  }

foreign import data Tree :: (* -> *) -> * -> *

mkTree :: forall f p'. (Eq p') => Lazy (HTML (Tree f p') (f Unit)) -> Tree f Unit
mkTree html =
  mkTree'
    { slot: unit
    , html: html
    , eq: eq
    , thunk: false
    }

mkTree' :: forall f p p'. TreeF f p p' -> Tree f p
mkTree' = unsafeCoerce

runTree :: forall f p r. (forall p'. TreeF f p p' -> r) -> Tree f p -> r
runTree k t = case unsafeCoerce t of tree -> k tree

graftTree :: forall f f' p p'. Natural f f' -> (p -> p') -> Tree f p -> Tree f' p'
graftTree l r = runTree \t ->
  mkTree'
    { slot: r t.slot
    , html: bimap (graftTree l id) l <$> t.html
    , eq: t.eq
    , thunk: t.thunk
    }

thunkTree :: forall f p. Tree f p -> Tree f p
thunkTree = runTree (mkTree' <<< _ { thunk = true })

emptyTree :: forall f. Tree f Unit
emptyTree =
  mkTree'
    { slot: unit
    , html: defer \_ -> Text ""
    , eq: \_ _ -> false
    , thunk: false
    }
