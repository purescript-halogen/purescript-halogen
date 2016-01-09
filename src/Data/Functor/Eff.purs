module Data.Functor.Eff where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Cont.Trans (ContT())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class as EF
import Control.Monad.Except.Trans (ExceptT())
import Control.Monad.Free (Free(), liftF)
import Control.Monad.List.Trans (ListT())
import Control.Monad.Maybe.Trans (MaybeT())
import Control.Monad.Reader.Trans (ReaderT())
import Control.Monad.RWS.Trans (RWST())
import Control.Monad.State.Trans (StateT())
import Control.Monad.Trans (lift)
import Control.Monad.Writer.Trans (WriterT())

import Data.Monoid (Monoid)

class (Functor f) <= FunctorEff eff f where
  liftEff :: forall a. Eff eff a -> f a

instance functorEffEff :: FunctorEff eff (Eff eff) where
  liftEff = id

instance functorEffAff :: FunctorEff eff (Aff eff) where
  liftEff = EF.liftEff

instance functorEffFree :: (FunctorEff eff f) => FunctorEff eff (Free f) where
  liftEff = liftF <<< liftEff

instance monadAffContT :: (Monad m, FunctorEff eff m) => FunctorEff eff (ContT r m) where
  liftEff = lift <<< liftEff

instance monadAffExceptT :: (Monad m, FunctorEff eff m) => FunctorEff eff (ExceptT e m) where
  liftEff = lift <<< liftEff

instance monadAffListT :: (Monad m, FunctorEff eff m) => FunctorEff eff (ListT m) where
  liftEff = lift <<< liftEff

instance monadAffMaybe :: (Monad m, FunctorEff eff m) => FunctorEff eff (MaybeT m) where
  liftEff = lift <<< liftEff

instance monadAffReader :: (Monad m, FunctorEff eff m) => FunctorEff eff (ReaderT r m) where
  liftEff = lift <<< liftEff

instance monadAffRWS :: (Monad m, Monoid w, FunctorEff eff m) => FunctorEff eff (RWST r w s m) where
  liftEff = lift <<< liftEff

instance monadAffState :: (Monad m, FunctorEff eff m) => FunctorEff eff (StateT s m) where
  liftEff = lift <<< liftEff

instance monadAffWriter :: (Monad m, Monoid w, FunctorEff eff m) => FunctorEff eff (WriterT w m) where
  liftEff = lift <<< liftEff
