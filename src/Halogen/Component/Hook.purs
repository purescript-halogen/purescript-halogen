module Halogen.Component.Hook
  ( Hook(..)
  , Finalized
  , finalized
  , runFinalized
  , mapFinalized
  , lmapHook
  , rmapHook
  ) where

import Prelude
import Control.Monad.Free (Free, hoistFree)
import Halogen.Query.HalogenF (HalogenF, hoistHalogenF)
import Unsafe.Coerce (unsafeCoerce)

data Hook f m
  = PostRender (f Unit)
  | Finalized (Finalized m)

data FinalizedF s f g m p = FinalizedF (f ~> Free (HalogenF s f g m p)) s (f Unit)

foreign import data Finalized :: (* -> *) -> *

finalized
  :: forall s f g m p
   . (f ~> Free (HalogenF s f g m p))
  -> s
  -> f Unit
  -> Finalized m
finalized e s i = unsafeCoerce (FinalizedF e s i)

runFinalized
  :: forall m r
   . (forall s f g p. f ~> Free (HalogenF s f g m p) -> s -> f Unit -> r)
  -> Finalized m
  -> r
runFinalized k f =
  case unsafeCoerce f of
    FinalizedF e s i -> k e s i

mapFinalized
  :: forall g g'
   . Functor g'
  => g ~> g'
  -> Finalized g
  -> Finalized g'
mapFinalized g =
  runFinalized \e s i -> finalized (hoistFree (hoistHalogenF g) <<< e) s i

lmapHook
  :: forall f f' g
   . f ~> f'
  -> Hook f g
  -> Hook f' g
lmapHook f (PostRender a) = PostRender (f a)
lmapHook _ (Finalized a) = Finalized a

rmapHook
  :: forall f g g'
   . Functor g'
  => g ~> g'
  -> Hook f g
  -> Hook f g'
rmapHook g (Finalized a) = Finalized (mapFinalized g a)
rmapHook _ (PostRender a) = PostRender a
