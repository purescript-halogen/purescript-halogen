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

newtype Emitter m f = Emitter (Step (f Unit) -> m Unit)

unEmitter :: forall m f. Emitter m f -> Step (f Unit) -> m Unit
unEmitter (Emitter f) = f

hoistEmitter :: forall m n f. (m Unit -> n Unit) -> Emitter m f -> Emitter n f
hoistEmitter nat (Emitter f) = Emitter (nat <<< f)

emit :: forall m f. Emitter m f -> (Unit -> f Unit) -> m Unit
emit (Emitter f) q = f (Emit (q unit))

close :: forall m a. Emitter m a -> m Unit
close (Emitter f) = f Close

data Step a = Emit a | Close

eitherFromStep :: forall a r. Step a -> Either a Unit
eitherFromStep = case _ of
  Emit a -> Left a
  Close -> Right unit

newtype Finalizer m = Finalizer (m Unit)

instance semigroupFinalizer :: Apply m => Semigroup (Finalizer m) where
  append (Finalizer a) (Finalizer b) = Finalizer (a *> b)

instance monoidFinalizer :: Applicative m => Monoid (Finalizer m) where
  mempty = Finalizer (pure unit)

hoistFinalizer :: forall m n. (m ~> n) -> Finalizer m -> Finalizer n
hoistFinalizer nat (Finalizer f) = Finalizer (nat f)

finalize :: forall m. Finalizer m -> m Unit
finalize (Finalizer a) = a

newtype EventSource m f = EventSource (m { producer :: CR.Producer (f Unit) m Unit, finalizer :: Finalizer m })

unEventSource :: forall m f. EventSource m f -> m { producer :: CR.Producer (f Unit) m Unit, finalizer :: Finalizer m }
unEventSource (EventSource e) = e

interpret :: forall m f g. Functor m => (f ~> g) -> EventSource m f -> EventSource m g
interpret f (EventSource es) =
  EventSource $
    map
      (\e -> { producer: FT.interpret (lmap f) e.producer, finalizer: e.finalizer })
      es

hoist :: forall m n a. Functor n => (m ~> n) -> EventSource m a -> EventSource n a
hoist nat (EventSource es) =
  EventSource $
    map
      (\e -> { producer: FT.hoistFreeT nat e.producer, finalizer: hoistFinalizer nat e.finalizer })
      (nat es)

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
      CR.producer $ map eitherFromStep (liftAff $ AV.takeVar inputVar)
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
