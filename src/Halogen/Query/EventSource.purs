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
import Effect.Aff (Aff, attempt, forkAff, runAff_)
import Effect.Aff.AVar as AV
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception as Exn
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class as Rec
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
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
  :: forall f m a
   . MonadAff m
  => ((a -> Effect Unit) -> Effect Unit)
  -> (a -> Maybe (f SubscribeStatus))
  -> EventSource f m
eventSource attach handler =
  EventSource
    let producer = produce \emit -> attach (emit <<< Left <<< handler)
    in pure { producer: FT.hoistFreeT liftAff $ catMaybes producer, done: pure unit }

-- | Similar to `eventSource` but allows the attachment function to return an
-- | action to perform when the handler is detached.
eventSource'
  :: forall f m a
   . MonadAff m
  => ((a -> Effect Unit) -> Effect (Effect Unit))
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
  :: forall f m
   . MonadAff m
  => (Effect Unit -> Effect Unit)
  -> f SubscribeStatus
  -> EventSource f m
eventSource_ attach query =
  EventSource
    let producer = produce \emit -> attach $ emit (Left query)
    in pure { producer: FT.hoistFreeT liftAff producer, done: pure unit }

-- | Similar to `eventSource_` but allows the attachment function to return an
-- | action to perform when the handler is detached.
eventSource_'
  :: forall f m
   . MonadAff m
  => (Effect Unit -> Effect (Effect Unit))
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
  :: forall a r
   . ((Either a r -> Effect Unit) -> Effect Unit)
  -> CR.Producer a Aff r
produce recv = produceAff (\send ->
  liftEffect (recv (void <<< runAff' <<< send)))

produce'
  :: forall a r
   . ((Either a r -> Effect Unit) -> Effect (Effect Unit))
  -> Aff { producer :: CR.Producer a Aff r, cancel :: r -> Aff Boolean }
produce' recv =
  produceAff' \send -> do
    x <- liftEffect $ recv (void <<< runAff' <<< send)
    pure (liftEffect x)

runAff' :: Aff Unit -> Effect Unit
runAff' = runAff_ (either (const (pure unit)) pure)

produceAff
  :: forall a r m
   . MonadAff m
  => ((Either a r -> Aff Unit) -> Aff Unit)
  -> CR.Producer a m r
produceAff recv = do
  v <- lift $ liftAff AV.empty
  void $ lift $ liftAff $ forkAff $ recv $ flip AV.put v
  CR.producer $ liftAff $ AV.take v

produceAff'
  :: forall a r
   . ((Either a r -> Aff Unit) -> Aff (Aff Unit))
  -> Aff { producer :: CR.Producer a Aff r, cancel :: r -> Aff Boolean }
produceAff' recv = do
  inputVar <- AV.empty
  finalizeVar <- AV.empty
  let
    producer = do
      lift $ flip AV.put finalizeVar =<< recv (flip AV.put inputVar)
      CR.producer (AV.take inputVar)
    cancel r =
      attempt (AV.take finalizeVar) >>= case _ of
        Left _ -> pure false
        Right finalizer -> do
          AV.kill (Exn.error "finalized") finalizeVar
          finalizer
          pure true
  pure { producer, cancel }
