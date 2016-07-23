module Halogen.Query.EventSource
  ( EventSource(..)
  , runEventSource
  , eventSource
  , eventSource_
  , catEventSource
  , ParentEventSource()
  , toParentEventSource
  , fromParentEventSource
  ) where

import Prelude

import Control.Coroutine.Aff as CCA
import Control.Coroutine.Stalling as SCR
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Monad.Rec.Class (class MonadRec)

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Maybe (Maybe(..))

import Unsafe.Coerce as U

newtype EventSource f g = EventSource (SCR.StallingProducer (f Unit) g Unit)

runEventSource :: forall f g. EventSource f g -> SCR.StallingProducer (f Unit) g Unit
runEventSource (EventSource es) = es

-- | Creates an `EventSource` for an event listener that accepts one argument.
-- |
-- | - The first argument is the function that attaches the event listener.
-- | - The second argument is a handler that produces a value in `f`.
-- |
-- | For example:
-- |
-- | ``` purescript
-- | let onCopied = eventSource (Editor.onCopy editor) \text -> do
-- |       pure $ actionF (TextCopied text)
-- | ```
-- | (Taken from the Ace component example)
eventSource
  :: forall eff a f g
   . (Monad g, Affable (avar :: AVAR | eff) g)
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Eff (avar :: AVAR | eff) (f Unit))
  -> EventSource f g
eventSource attach handle =
  EventSource $ produce \emit ->
    attach (emit <<< Left <=< handle)

-- | Creates an `EventSource` for an event listener that accepts no arguments.
-- |
-- | - The first argument is the function that attaches the event listener.
-- | - The second argument is a handler that produces a value in `f`.
-- |
-- | For example:
-- |
-- | ``` purescript
-- | let onChange = eventSource_ (Session.onChange session) do
-- |       text <- Editor.getValue editor
-- |       pure $ actionF (ChangeText text)
-- | ```
-- | (Taken from the Ace component example)
eventSource_
  :: forall eff f g
   . (Monad g, Affable (avar :: AVAR | eff) g)
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> Eff (avar :: AVAR | eff) (f Unit)
  -> EventSource f g
eventSource_ attach handle =
  EventSource $ produce \emit ->
    attach (emit <<< Left =<< handle)

produce
  :: forall a r m eff
   . (Functor m, Affable (avar :: AVAR | eff) m)
  => ((Either a r -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> SCR.StallingProducer a m r
produce = SCR.producerToStallingProducer <<< hoistFreeT fromAff <<< CCA.produce

-- | Take an `EventSource` with events in `1 + f` to one with events in `f`.
-- | This is useful for simultaneously filtering and handling events.
catEventSource
  :: forall f g
   . MonadRec g
  => EventSource (Coproduct (Const Unit) f) g
  -> EventSource f g
catEventSource (EventSource es) =
  EventSource $
    SCR.catMaybes $
      SCR.mapStallingProducer (coproduct (const Nothing) Just) es

foreign import data ParentEventSource :: (* -> *) -> (* -> *) -> *

toParentEventSource :: forall f g h. EventSource f h -> ParentEventSource f (Free (g h))
toParentEventSource = U.unsafeCoerce

fromParentEventSource :: forall f g h. ParentEventSource f (Free (g h)) -> EventSource f h
fromParentEventSource = U.unsafeCoerce
