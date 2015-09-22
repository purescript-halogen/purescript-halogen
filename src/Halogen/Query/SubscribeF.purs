-- | A part of the `HalogenF` algebra that allows subscription to event
-- | listeners.
module Halogen.Query.SubscribeF
  ( EventSource()
  , eventSource
  , eventSource_
  , SubscribeF(..)
  , remapSubscribe
  , hoistSubscribe
  , subscribeN
  ) where

import Prelude

import Control.Bind ((<=<), (=<<))
import Control.Coroutine (Producer(), Consumer(), runProcess, emit, ($$))
import Control.Coroutine.Aff (produce)
import Control.Monad.Free.Trans (hoistFreeT, interpret)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff)
import Control.Monad.Rec.Class (MonadRec)

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.NaturalTransformation (Natural())

-- | A type alias for a coroutine producer used to represent a subscribable
-- | source of events.
type EventSource f g = Producer (f Unit) g Unit

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
eventSource :: forall eff a f. ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
                            -> (a -> Eff (avar :: AVAR | eff) (f Unit))
                            -> EventSource f (Aff (avar :: AVAR | eff))
eventSource attach handle = produce \emit -> attach (emit <<< Left <=< handle)

-- | Creates an `EventSource` for an event listener that accepts no arguments.
-- |
-- | - The first argument is the function that attaches the event listener.
-- | - The second argument is a handler that produces a value in `f`.
-- |
-- | For example:
-- |
-- | ``` purescript
-- | let onChange = eventSource_ (Session.onChange session) do
-- |       text <- liftEff $ Editor.getValue editor
-- |       pure $ actionF (ChangeText text)
-- | ```
-- | (Taken from the Ace component example)
eventSource_ :: forall eff f. (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
                           -> Eff (avar :: AVAR | eff) (f Unit)
                           -> EventSource f (Aff (avar :: AVAR | eff))
eventSource_ attach handle = produce \emit -> attach (emit <<< Left =<< handle)

-- | The subscribe algebra.
data SubscribeF f g a = Subscribe (EventSource f g) a

instance functorSubscribeF :: Functor (SubscribeF f g) where
  map f (Subscribe p next) = Subscribe p (f next)

-- | Changes the generating functor for an `EventSource`. Used internally by
-- | Halogen when installing components.
remapSubscribe :: forall f g h a. (Functor h) => (Natural f g) -> SubscribeF f h a -> SubscribeF g h a
remapSubscribe nat (Subscribe p next) = Subscribe (interpret (lmap nat) p) next

-- | Changes the underlying monad for an `EventSource`. Used internally by
-- | Halogen when installing components.
hoistSubscribe :: forall f g h a. (Functor h) => (Natural g h) -> SubscribeF f g a -> SubscribeF f h a
hoistSubscribe nat (Subscribe p next) = Subscribe (hoistFreeT nat p) next

-- | A natural transformation for interpreting the subscribe algebra as its
-- | underlying monad, via a coroutine consumer. Used internally by Halogen in
-- | component installation and `runUI`.
subscribeN :: forall f g. (MonadRec g) => Consumer (f Unit) g Unit -> Natural (SubscribeF f g) g
subscribeN c (Subscribe p next) = runProcess (p $$ c) $> next
