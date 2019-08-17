module Halogen.Surface where

import Unsafe.Coerce (unsafeCoerce)

class Surface (s :: Type -> Type -> Type) e | s -> e

foreign import data SurfaceElement :: (Type -> Type -> Type) -> Type

box ∷ ∀ s e. Surface s e => e -> SurfaceElement s
box = unsafeCoerce

unbox ∷ ∀ s e. Surface s e => SurfaceElement s -> e
unbox = unsafeCoerce
