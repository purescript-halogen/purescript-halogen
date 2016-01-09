module Data.Functor.Aff where

import Prelude
import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free(), liftF)

class (Functor f) <= FunctorAff eff f where
  liftAff :: forall a. Aff eff a -> f a

instance functorAffAff :: FunctorAff eff (Aff eff) where
  liftAff = id

instance functorAffFree :: (FunctorAff eff f) => FunctorAff eff (Free f) where
  liftAff = liftF <<< liftAff
