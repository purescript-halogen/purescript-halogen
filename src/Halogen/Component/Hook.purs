module Halogen.Component.Hook
  ( Hook(..)
  , Finalized()
  , finalized
  , runFinalized
  , mapFinalized
  , lmapHook
  , rmapHook
  ) where

import Prelude
import Control.Monad.Free (Free(), mapF)
import Data.NaturalTransformation (Natural())
import Halogen.Query.HalogenF (HalogenF(), hoistHalogenF)
import Unsafe.Coerce (unsafeCoerce)

data Hook f g
  = PostRender (f Unit)
  | Finalized (Finalized g)

data FinalizedF s f g = FinalizedF (Natural f (Free (HalogenF s f g))) s (f Unit)

foreign import data Finalized :: (* -> *) -> *

finalized
  :: forall s f g
   . Natural f (Free (HalogenF s f g))
  -> s
  -> f Unit
  -> Finalized g
finalized e s i = unsafeCoerce (FinalizedF e s i)

runFinalized
  :: forall g r
   . (forall s f. Natural f (Free (HalogenF s f g)) -> s -> f Unit -> r)
  -> Finalized g
  -> r
runFinalized k f =
  case unsafeCoerce f of
    FinalizedF e s i -> k e s i

mapFinalized
  :: forall g g'
   . (Functor g')
  => Natural g g'
  -> Finalized g
  -> Finalized g'
mapFinalized g =
  runFinalized \e s i -> finalized (mapF (hoistHalogenF g) <<< e) s i

lmapHook
  :: forall f f' g
   . Natural f f'
  -> Hook f g
  -> Hook f' g
lmapHook f (PostRender a) = PostRender (f a)
lmapHook _ (Finalized a) = Finalized a

rmapHook
  :: forall f g g'
   . (Functor g')
  => Natural g g'
  -> Hook f g
  -> Hook f g'
rmapHook g (Finalized a) = Finalized (mapFinalized g a)
rmapHook _ (PostRender a) = PostRender a

