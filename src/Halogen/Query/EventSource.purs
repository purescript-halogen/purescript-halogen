module Halogen.Query.EventSource
  ( EventSource(..)
  , runEventSource
  , eventSource
  , eventSource_
  ) where

import Prelude

import Control.Coroutine.Aff (produce)
import Control.Coroutine as CR
import Control.Monad.Free.Trans as FT
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Foldable (for_)

newtype EventSource f g = EventSource (CR.Producer (f Unit) g Unit)

runEventSource :: forall f g. EventSource f g -> CR.Producer (f Unit) g Unit
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
  :: forall eff a f m
   . (MonadAff (avar :: AVAR | eff) m)
  => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Eff (avar :: AVAR | eff) (Maybe (f Unit)))
  -> EventSource f m
eventSource attach handle =
  EventSource $ FT.hoistFreeT liftAff $ catMaybes $ produce \emit ->
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
  :: forall eff f m
   . (MonadAff (avar :: AVAR | eff) m)
  => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> Eff (avar :: AVAR | eff) (Maybe (f Unit))
  -> EventSource f m
eventSource_ attach handle =
  EventSource $ FT.hoistFreeT liftAff $ catMaybes $ produce \emit ->
    attach (emit <<< Left =<< handle)

catMaybes
  :: forall m a r
   . MonadRec m
  => CR.Producer (Maybe a) m r
  -> CR.Producer a m r
catMaybes =
  tailRecM $
    FT.resume >>> lift >=> case _ of
      Left r -> pure $ Done r
      Right (CR.Emit o next) ->
        for_ o CR.emit $> Loop next
