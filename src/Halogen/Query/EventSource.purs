module Halogen.Query.EventSource
  ( EventSource(..)
  , eventSource
  , eventSource_
  , catMaybes
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Free.Trans as FT
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
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
  :: forall eff a f m
   . MonadAff (avar :: AVAR | eff) m
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Eff (avar :: AVAR | eff) (Maybe (f Unit)))
  -> EventSource f m
eventSource attach handle =
  EventSource $ FT.hoistFreeT liftAff $ catMaybes $ produce \emit ->
    attach (emit <<< Left <=< handle)

-- | Creates an `EventSource` for an event listener that accepts no arguments.
-- |
-- | - The first argument is the function that attaches the event listener.
-- | - The second argument is a handler that optionally produces a value in `f`.
eventSource_
  :: forall eff f m
   . MonadAff (avar :: AVAR | eff) m
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> Eff (avar :: AVAR | eff) (Maybe (f Unit))
  -> EventSource f m
eventSource_ attach handle =
  EventSource $ FT.hoistFreeT liftAff $ catMaybes $ produce \emit ->
    attach (emit <<< Left =<< handle)

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
