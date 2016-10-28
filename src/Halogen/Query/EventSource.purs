module Halogen.Query.EventSource
  ( EventSource(..)
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
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..), isLeft, isRight)
import Data.Foldable (for_)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype EventSource f g = EventSource (CR.Producer (f Unit) g Unit)

instance newtypeEventSource :: Newtype (EventSource f g) (FT.FreeT (CR.Emit (f Unit)) g Unit) where
  wrap = EventSource
  unwrap (EventSource cr) = cr

-- | Creates an `EventSource` for an event listener that accepts one argument.
-- |
-- | - The first argument is the function that attaches the event listener.
-- | - The second argument is a handler that optionally produces a value in `f`.
eventSource
  :: forall f m a eff
   . MonadAff (avar :: AVAR | eff) m
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Eff (avar :: AVAR | eff) (Maybe (f Unit)))
  -> EventSource f m
eventSource attach handle =
  let producer = produce \emit -> attach (emit <<< Left <=< handle)
  in EventSource $ FT.hoistFreeT liftAff $ catMaybes producer

-- | Similar to `eventSource` but allows the attachment function to return an
-- | action to perform to detach the handler also. This allows the `eventSource`
-- | to be unsubscribed from.
eventSource'
  :: forall f m a eff
   . MonadAff (avar :: AVAR | eff) m
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> (a -> Eff (avar :: AVAR | eff) (Maybe (f Unit)))
  -> m { eventSource :: EventSource f m, unsubscribe :: m Unit }
eventSource' attach handle = do
  { producer, cancel } <- liftAff $ produce' (\emit -> attach (emit <<< Left <=< handle))
  pure
    { eventSource: EventSource $ FT.hoistFreeT liftAff $ catMaybes producer
    , unsubscribe: liftAff (cancel unit $> unit)
    }

-- | Creates an `EventSource` for an event listener that accepts no arguments.
-- |
-- | - The first argument is the function that attaches the event listener.
-- | - The second argument is a handler that optionally produces a value in `f`.
eventSource_
  :: forall f m eff
   . MonadAff (avar :: AVAR | eff) m
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> Eff (avar :: AVAR | eff) (Maybe (f Unit))
  -> EventSource f m
eventSource_ attach handle =
  EventSource $ FT.hoistFreeT liftAff $ catMaybes $ produce \emit ->
    attach (emit <<< Left =<< handle)

-- | Similar to `eventSource_` but allows the attachment function to return an
-- | action to perform to detach the handler also. This allows the `eventSource`
-- | to be unsubscribed from.
eventSource_'
  :: forall f m eff
   . MonadAff (avar :: AVAR | eff) m
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> Eff (avar :: AVAR | eff) (Maybe (f Unit))
  -> m { eventSource :: EventSource f m, unsubscribe :: m Unit }
eventSource_' attach handle = do
  { producer, cancel } <- liftAff $ produce' (\emit -> attach (emit <<< Left =<< handle))
  pure
    { eventSource: EventSource $ FT.hoistFreeT liftAff $ catMaybes producer
    , unsubscribe: liftAff (cancel unit $> unit)
    }

-- | Takes a producer of `Maybe`s and filters out the `Nothings`. Useful for
-- | constructing `EventSource`s for producers that don't need to handle every
-- | event.
catMaybes
  :: forall m a r
   . MonadRec m
  => CR.Producer (Maybe a) m r
  -> CR.Producer a m r
catMaybes =
  tailRecM $ FT.resume >>> lift >=> case _ of
    Left r -> pure $ Done r
    Right (CR.Emit ma next) -> for_ ma CR.emit $> Loop next

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
  lift $ liftAff $ forkAff $ recv $ AV.putVar v
  CR.producer $ liftAff $ AV.takeVar v

produceAff'
  :: forall a r eff
   . ((Either a r -> Aff (avar :: AVAR | eff) Unit) -> Aff (avar :: AVAR | eff) (Aff (avar :: AVAR | eff) Unit))
  -> Aff (avar :: AVAR | eff) { producer :: CR.Producer a (Aff (avar :: AVAR | eff)) r, cancel :: r -> Aff (avar :: AVAR | eff) Boolean }
produceAff' recv = do
  inputVar <- AV.makeVar
  finalizeVar <- AV.makeVar
  let
    pro = do
      lift $ forkAff do
        done <- recv $ AV.putVar inputVar
        AV.putVar finalizeVar done
      CR.producer do
        out <- AV.takeVar inputVar
        when (isRight out) do
          AV.killVar inputVar (Exn.error "ended")
          join $ AV.takeVar finalizeVar
        pure out
    cancel r = pure <<< isLeft =<< attempt (AV.putVar inputVar (Right r))
  pure { producer: pro, cancel: cancel }
