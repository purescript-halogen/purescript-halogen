module Data.Functor.Aff where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Cont.Trans (ContT())
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

class (Functor f) <= FunctorAff eff f where
  liftAff :: forall a. Aff eff a -> f a

instance functorAffAff :: FunctorAff eff (Aff eff) where
  liftAff = id

instance functorAffFree :: (FunctorAff eff f) => FunctorAff eff (Free f) where
  liftAff = liftF <<< liftAff

instance monadAffContT :: (Monad m, FunctorAff eff m) => FunctorAff eff (ContT r m) where
  liftAff = lift <<< liftAff

instance monadAffExceptT :: (Monad m, FunctorAff eff m) => FunctorAff eff (ExceptT e m) where
  liftAff = lift <<< liftAff

instance monadAffListT :: (Monad m, FunctorAff eff m) => FunctorAff eff (ListT m) where
  liftAff = lift <<< liftAff

instance monadAffMaybe :: (Monad m, FunctorAff eff m) => FunctorAff eff (MaybeT m) where
  liftAff = lift <<< liftAff

instance monadAffReader :: (Monad m, FunctorAff eff m) => FunctorAff eff (ReaderT r m) where
  liftAff = lift <<< liftAff

instance monadAffRWS :: (Monad m, Monoid w, FunctorAff eff m) => FunctorAff eff (RWST r w s m) where
  liftAff = lift <<< liftAff

instance monadAffState :: (Monad m, FunctorAff eff m) => FunctorAff eff (StateT s m) where
  liftAff = lift <<< liftAff

instance monadAffWriter :: (Monad m, Monoid w, FunctorAff eff m) => FunctorAff eff (WriterT w m) where
  liftAff = lift <<< liftAff
