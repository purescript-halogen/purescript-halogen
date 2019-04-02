module Test.DocExamples.EventSource where

import Prelude

import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff as Aff
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource

data Action = Notify String

myEventSource :: EventSource Aff Action
myEventSource = EventSource.affEventSource \emitter -> do
  Aff.delay (Milliseconds 1000.0)
  EventSource.emit emitter (Notify "hello")
  pure mempty
