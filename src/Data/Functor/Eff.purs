module Data.Functor.Eff where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class as EF
import Control.Monad.Aff (Aff())
import Control.Monad.Free (Free(), liftF)

class (Functor f) <= FunctorEff eff f where
  liftEff :: forall a. Eff eff a -> f a

instance functorEffEff :: FunctorEff eff (Eff eff) where
  liftEff = id

instance functorEffAff :: FunctorEff eff (Aff eff) where
  liftEff = EF.liftEff

instance functorEffFree :: (FunctorEff eff f) => FunctorEff eff (Free f) where
  liftEff = liftF <<< liftEff
