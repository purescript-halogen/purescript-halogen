module Halogen.Query.EventSource
  ( EventSource(..)
  , SubscribeStatus(..)
  , unEventSource
  , interpret
  , hoist
  , eventSource
  , eventSource'
  , eventSource_
  , eventSource_'
  , catMaybes
  , produce
  , produce'
  , produceAff
  , produceAff'
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff, runAff, forkAff, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class as Rec
import Control.Monad.Trans.Class (lift)

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe)

newtype EventSource f m = EventSource (m { producer :: CR.Producer (f SubscribeStatus) m Unit, done :: m Unit })

unEventSource :: forall f m. EventSource f m -> m { producer :: CR.Producer (f SubscribeStatus) m Unit, done :: m Unit }
unEventSource (EventSource e) = e

interpret :: forall f g m. Functor m => (f ~> g) -> EventSource f m -> EventSource g m
interpret nat (EventSource es) =
  EventSource $
    map
      (\e -> { producer: FT.interpret (lmap nat) e.producer, done: e.done })
      es

hoist :: forall f m n. Functor n => (m ~> n) -> EventSource f m -> EventSource f n
hoist nat (EventSource es) =
  EventSource $
    map
      (\e -> { producer: FT.hoistFreeT nat e.producer, done: nat e.done })
      (nat es)

-- | The status of an `EventSource` subscription. When a query raised by an
-- | `EventSource` evaluates to `Done` the producer will be unsubscribed from.
data SubscribeStatus
  = Listening
  | Done

derive instance eqSubscribeStatus :: Eq SubscribeStatus
derive instance ordSubscribeStatus :: Ord SubscribeStatus

-- | Creates an `EventSource` for a callback that accepts one argument.
-- |
-- | - The first argument is the function that attaches the listener.
-- | - The second argument is a handler that optionally produces a value in `f`.
eventSource
  :: forall f m a eff
   . MonadAff (avar :: AVAR | eff) m
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Maybe (f SubscribeStatus))
  -> EventSource f m
eventSource attach handler =
  EventSource
    let producer = produce \emit -> attach (emit <<< Left <<< handler)
    in pure { producer: FT.hoistFreeT liftAff $ catMaybes producer, done: pure unit }

-- | Similar to `eventSource` but allows the attachment function to return an
-- | action to perform when the handler is detached.
eventSource'
  :: forall f m a eff
   . MonadAff (avar :: AVAR | eff) m
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> (a -> Maybe (f SubscribeStatus))
  -> EventSource f m
eventSource' attach handler = do
  EventSource do
    { producer, cancel } <- liftAff $ produce' \emit -> attach (emit <<< Left <<< handler)
    pure
      { producer: FT.hoistFreeT liftAff $ catMaybes producer
      , done: liftAff $ void $ cancel unit
      }

-- | Creates an `EventSource` for a callback that accepts no arguments.
-- |
-- | - The first argument is the function that attaches the listener.
-- | - The second argument is the query to raise whenever the listener is
-- |   triggered.
eventSource_
  :: forall f m eff
   . MonadAff (avar :: AVAR | eff) m
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> f SubscribeStatus
  -> EventSource f m
eventSource_ attach query =
  EventSource
    let producer = produce \emit -> attach $ emit (Left query)
    in pure { producer: FT.hoistFreeT liftAff producer, done: pure unit }

-- | Similar to `eventSource_` but allows the attachment function to return an
-- | action to perform when the handler is detached.
eventSource_'
  :: forall f m eff
   . MonadAff (avar :: AVAR | eff) m
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> f SubscribeStatus
  -> EventSource f m
eventSource_' attach query =
  EventSource do
    { producer, cancel } <- liftAff $ produce' \emit -> attach $ emit (Left query)
    pure
      { producer: FT.hoistFreeT liftAff producer
      , done: liftAff $ void $ cancel unit
      }

-- | Takes a producer of `Maybe`s and filters out the `Nothings`. Useful for
-- | constructing `EventSource`s for producers that don't need to handle every
-- | event.
catMaybes
  :: forall m a r
   . Rec.MonadRec m
  => CR.Producer (Maybe a) m r
  -> CR.Producer a m r
catMaybes =
  Rec.tailRecM $ FT.resume >>> lift >=> case _ of
    Left r -> pure $ Rec.Done r
    Right (CR.Emit ma next) -> for_ ma CR.emit $> Rec.Loop next

produce
  :: forall a r eff
   . ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> CR.Producer a (Aff (avar :: AVAR | eff)) r
produce recv = produceAff (\send ->
  liftEff (recv (void <<< runAff (const (pure unit)) pure <<< send)))

produce'
  :: forall a r eff
   . ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> Aff (avar :: AVAR | eff) { producer :: CR.Producer a (Aff (avar :: AVAR | eff)) r, cancel :: r -> Aff (avar :: AVAR | eff) Boolean }
produce' recv =
  produceAff' \send -> do
    x <- liftEff $ recv (void <<< runAff (const (pure unit)) pure <<< send)
    pure (liftEff x)

produceAff
  :: forall a r eff m
   . MonadAff (avar :: AVAR | eff) m
  => ((Either a r -> Aff (avar :: AVAR | eff) Unit) -> Aff (avar :: AVAR | eff) Unit)
  -> CR.Producer a m r
produceAff recv = do
  v <- lift $ liftAff AV.makeVar
  void $ lift $ liftAff $ forkAff $ recv $ AV.putVar v
  CR.producer $ liftAff $ AV.takeVar v

produceAff'
  :: forall a r eff
   . ((Either a r -> Aff (avar :: AVAR | eff) Unit) -> Aff (avar :: AVAR | eff) (Aff (avar :: AVAR | eff) Unit))
  -> Aff (avar :: AVAR | eff) { producer :: CR.Producer a (Aff (avar :: AVAR | eff)) r, cancel :: r -> Aff (avar :: AVAR | eff) Boolean }
produceAff' recv = do
  inputVar <- AV.makeVar
  finalizeVar <- AV.makeVar
  let
    producer = do
      lift $ AV.putVar finalizeVar =<< recv (AV.putVar inputVar)
      CR.producer (AV.takeVar inputVar)
    cancel r =
      attempt (AV.takeVar finalizeVar) >>= case _ of
        Left _ -> pure false
        Right finalizer -> do
          AV.killVar finalizeVar (Exn.error "finalized")
          finalizer
          pure true
  pure { producer, cancel }
