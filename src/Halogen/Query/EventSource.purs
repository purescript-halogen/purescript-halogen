module Halogen.Query.EventSource where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Free.Trans as FT
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Monoid (class Monoid)
import Data.Profunctor (dimap)

newtype EventSource m f = EventSource (m { producer :: CR.Producer (f Unit) m Unit, finalizer :: Finalizer m })

affEventSource
  :: forall m f eff
   . MonadAff (avar :: AVAR | eff) m
  => (Emitter (Aff (avar :: AVAR | eff)) f -> Aff (avar :: AVAR | eff) (Finalizer (Aff (avar :: AVAR | eff))))
  -> EventSource m f
affEventSource recv = EventSource $ liftAff do
  inputVar <- AV.makeEmptyVar
  finalizeVar <- AV.makeEmptyVar
  let
    producer = do
      lift $ liftAff $ flip AV.putVar finalizeVar =<< recv (Emitter (flip AV.putVar inputVar))
      CR.producer $ liftAff $ AV.takeVar inputVar
    finalizer = Finalizer do
      liftAff (attempt (AV.takeVar finalizeVar)) >>= case _ of
        Left _ -> pure unit
        Right finalizer -> liftAff do
          AV.killVar (Exn.error "finalized") finalizeVar
          finalize finalizer
  pure { producer, finalizer }

effEventSource
  :: forall m f eff
   . MonadAff (avar :: AVAR | eff) m
  => (Emitter (Eff (avar :: AVAR | eff)) f -> Eff (avar :: AVAR | eff) (Finalizer (Eff (avar :: AVAR | eff))))
  -> EventSource m f
effEventSource = affEventSource <<< dimap (hoistEmitter launchAff_) (liftEff <<< map (hoistFinalizer liftEff))

interpret :: forall m f g. Functor m => (f ~> g) -> EventSource m f -> EventSource m g
interpret f (EventSource es) =
  EventSource $
    map
      (\e -> { producer: FT.interpret (lmap f) e.producer, finalizer: e.finalizer })
      es

hoist :: forall m n f. Functor n => (m ~> n) -> EventSource m f -> EventSource n f
hoist nat (EventSource es) =
  EventSource $
    map
      (\e -> { producer: FT.hoistFreeT nat e.producer, finalizer: hoistFinalizer nat e.finalizer })
      (nat es)

newtype Emitter m f = Emitter (Either (f Unit) Unit -> m Unit)

emit :: forall m f. Emitter m f -> (Unit -> f Unit) -> m Unit
emit (Emitter f) q = f (Left (q unit))

close :: forall m a. Emitter m a -> m Unit
close (Emitter f) = f (Right unit)

hoistEmitter :: forall m n f. (m Unit -> n Unit) -> Emitter m f -> Emitter n f
hoistEmitter nat (Emitter f) = Emitter (nat <<< f)

newtype Finalizer m = Finalizer (m Unit)

instance semigroupFinalizer :: Apply m => Semigroup (Finalizer m) where
  append (Finalizer a) (Finalizer b) = Finalizer (a *> b)

instance monoidFinalizer :: Applicative m => Monoid (Finalizer m) where
  mempty = Finalizer (pure unit)

finalize :: forall m. Finalizer m -> m Unit
finalize (Finalizer a) = a

hoistFinalizer :: forall m n. (m ~> n) -> Finalizer m -> Finalizer n
hoistFinalizer nat (Finalizer a) = Finalizer (nat a)
