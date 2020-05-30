# Lifecycles and Subscriptions

The concepts you've learned so far cover the majority of Halogen components you'll write. Most components have internal state, render HTML elements, and respond by performing actions when users click, hover over, or otherwise interact with the rendered HTML.

But actions can arise internally from other kinds of events, too. Here are some common examples:

1. You need to run an action when the component starts up (for example, you need to perform an effect to get your initial state) or when the component is removed from the DOM (for example, to clean up resources you acquired). These are called **lifecycle events**.
2. You need to run an action at regular intervals (for example, you need to perform an update every 10 seconds), or when an event arises from outside your rendered HTML (for example, you need to run an action when a key is pressed on the DOM window, or you need to handle events that occur in a third-party component like a text editor). These are handled by **event source subscriptions**, sometimes just called **subscriptions** or **event sources**.

We'll learn about one other way actions can arise in a component when we learn about parent and child components in the next chapter. This chapter will focus on lifecycles and subscriptions.

## Lifecycle Events

Every Halogen component has access to two lifecycle events:

1. The component can evaluate an action it is initialized (Halogen creates it)
2. The component can evaluate an action when it is finalized (Halogen removes it)

We specify what action (if any) to run when the component is initialized and finalized as part of the `eval` function -- the same place where we've been providing the `handleAction` function. In the next section we'll get into more detail about what `eval` is, but first lets see an example of lifecycles in action.

The following example is nearly identical to our random number component, but with some important changes.

1. We have added `Initialize` and `Finalize` in addition to our existing `Regenerate` action.
2. We've expanded our `eval` to include an `initialize` field that states our `Initialize` action should be evaluated when the component initializes, and a `finalize` field that states our `Finalize` action should be evaluated when the component finalizes.
3. Since we have two new actions, we've added two new cases to our `handleAction` function to describe how to handle them.

Try reading through the example:

```purs
module Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Maybe Number

data Action
  = Initialize
  | Regenerate
  | Finalize

component :: forall q i o m. MonadEffect m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

initialState :: forall i. i -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let value = maybe "No number generated yet" show state
  HH.div_
    [ HH.h1_
        [ HH.text "Random number" ]
    , HH.p_
        [ HH.text ("Current value: " <> value) ]
    , HH.button
        [ HE.onClick \_ -> Just Regenerate ]
        [ HH.text "Generate new number" ]
    ]

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction Regenerate
    log ("Initialized: " <> show newNumber)

  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)

  Finalize -> do
    number <- H.get
    log ("Finalized! Last number was: " <> show number)
```

When this component mounts we'll generate a random number and log it to the console. We'll keep regenerating random numbers as the user clicks the button, and when this component is removed from the DOM it will log the last number it had in state.

We made one other interesting change in this example: in our `Initialize` handler we called `handleAction Regenerate` -- we called `handleAction` recursively. It can be convenient to call actions from within other actions from time to time as we've done here. We could have also inlined `Regenerate`'s handler -- the following code does the same thing:

```purs
  Initialize -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)
    log ("Initialized: " <> show newNumber)
```

Before we move on to subscriptions and event sources, let's talk more about the `eval` function.

## The `eval` Function, `mkEval`, and `EvalSpec`

We've been using `eval` in all of our components, but so far we've only handled actions arising from our Halogen HTML via the `handleAction` function. But the `eval` function can describe _all_ the ways our component can evaluate `HalogenM` code in response to events.

In the vast majority of cases you don't need to care much about all the types and functions involved in the component spec and eval spec described below, but we'll briefly break down the types so you have an idea of what's going on.

The `mkComponent` function takes a `ComponentSpec`, which is a record containing three fields:

```purs
H.mkComponent
  { initialState :: input -> state
  , render :: state -> H.ComponentHTML action slots m
  , eval :: H.HalogenQ query action input ~> H.HalogenM state action slots output m
  }
```

We've spent plenty of time with the `initialState` and `render` functions already. But the `eval` function may look strange -- what is `HalogenQ`, and how do functions like `handleAction` fit in? For now, we'll focus on the most common use of this function, but you can find the full details in the Concepts Reference.

The `eval` function describes how to handle events that arise in the component. It's usually constructed by applying the `mkEval` function to an `EvalSpec`, the same way we applied `mkComponent` to a `ComponentSpec` to produce a `Component`.

For convenience, Halogen provides an already-complete `EvalSpec` called `defaultEval`, which does nothing when an event arises in the component. By using this default value you can override just the values you care about, while leaving the rest of them doing nothing.

Here's how we've defined `eval` functions that only handle actions so far:

```purs
H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

-- assuming we've defined a `handleAction` function in scope...
handleAction = ...
```

You can override more fields, if you need to. For example, if you need to support an initializer then you would override the `initialize` field too:

```purs
H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
```

Let's take a quick look at the full type of `EvalSpec`:

```purs
type EvalSpec state query action slots input output m =
  { handleAction :: action -> HalogenM state action slots output m Unit
  , handleQuery :: forall a. query a -> HalogenM state action slots output m (Maybe a)
  , initialize :: Maybe action
  , receive :: input -> Maybe action
  , finalize :: Maybe action
  }
```

The `EvalSpec` covers all the types available internally in your component. Fortunately, you don't need to specify this type anywhere -- you can just provide a record to `mkEval`. We'll cover the `handleQuery` and `receive` functions as well as the `query` and `output` types in the next chapter, as they're only relevant for child components.

Since in normal use you'll override specific fields from `defaultEval` rather than write out a whole eval spec yourself, let's also look at what `defaultEval` implements for each of these functions:

```purs
defaultEval =
  { handleAction: const (pure unit)
  , handleQuery: const (pure Nothing) -- we'll learn about this when we cover child components
  , initialize: Nothing
  , receive: const Nothing -- we'll learn about this when we cover child components
  , finalize: Nothing
  }
```

Now, let's move to the other common source of internal events: event sources we've subscribed to.

## Subscriptions

Sometimes you need to handle events arising internally that don't come from a user interacting with the Halogen HTML you've rendered. Two common sources are time-based actions and events that happen on an element outside one you've rendered (like the browser window).

In Halogen these kinds of events come from **event sources**. Components can subscribe to event sources by providing an action that should run every time an event happens.

Event sources are usually created with one of these functions:

1. `effectEventSource` and`affEventSource` let you produce an event source from an `Effect` or `Aff` function, respectively.
2. `eventListenerEventSource` lets you produce an event source by attaching an event listener to the DOM, like attaching a resize event to the browser window.

An event source can be thought of as a stream of actions: actions can be produced at any time from the event source, and your component will evaluate those actions so long as it remains subscribed to the event source. It's common to use create an event source and subscribe to it when the component initializes, though you can subscribe or unsubscribe from an event source at any time.

Let's see two examples of event sources in action: an `Aff`-based timer that counts the seconds since the component mounted and an event-listener-based stream that reports keyboard events on the document.

### Implementing a Timer

Our first example will use an `Aff`-based timer to increment every second.

```purs
module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as EventSource

data Action = Initialize | Tick

type State = Int

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render seconds = HH.text ("You have been here for " <> show seconds <> " seconds")

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    _ <- H.subscribe timer
    pure unit

  Tick ->
    H.modify_ \state -> state + 1

timer :: forall m. MonadAff m => EventSource m Action
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    EventSource.emit emitter Tick

  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber
```

Almost all of this code should look familiar, but there are two new parts.

First, we've defined an event source that will emit a `Tick` action every second until it is closed:

```purs
timer :: forall m. MonadAff m => EventSource m Action
timer = EventSource.affEventSource \emitter -> do
  fiber <- Aff.forkAff $ forever do
    Aff.delay $ Milliseconds 1000.0
    EventSource.emit emitter Tick

  pure $ EventSource.Finalizer do
    Aff.killFiber (error "Event source finalized") fiber
```

The `affEventSource` and `effectEventSource` functions take a callback that provides you with an `Emitter`. You can use this with the `EventSource.emit` function to broadcast an action, or with the `EventSource.close` function to close the event source (this lets you close an event source from within, instead of having to wait for the event source to be closed by its subscriber or automatically when the component finalizes). You can return a cleanup function to run when the event source closes, called its finalizer. In this case, we use the finalizer to kill the fiber we forked to loop the count.

Second, we use the `subscribe` function from Halogen to attach to the event source:

```purs
  Initialize -> do
    _ <- H.subscribe timer
    pure unit
```

The `subscribe` function takes an event source as an argument and it returns a `SubscriptionId`. You can pass this `SubscriptionId` to the `unsubscribe` function at any point to close the event source, run its cleanup function, and stop listening to outputs from it. Components automatically unsubscribe from any event sources when the component finalizes, so we don't need to unsubscribe here.

You may also be interested in the [Ace editor example](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples/ace), which subscribes to events that happen inside a third-party JavaScript component and uses them to trigger actions in a Halogen component.

### Using Event Listeners As Event Sources

Another common reason to use event sources is when you need to react to events in the DOM that don't arise directly from HTML elements you control. For example, we might want to listen to events that happen on the document itself.

In the following example we subscribe to key events on the document, save any characters that are typed while holding the `Shift` key, and stop listening if the user hits the `Enter` key. It demonstrates using the `eventListenerEventSource` function to attach an event listener and using the `H.unsubscribe` function to choose when to clean it up.

There is also a corresponding [example of keyboard input](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples/keyboard-input) in the examples directory.

```purs
module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource (EventSource, eventListenerEventSource)
import Web.Event.Event as E
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = { chars :: String }

data Action
  = Initialize
  | HandleKey H.SubscriptionId KE.KeyboardEvent

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall i. i -> State
initialState _ = { chars: "" }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
    , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
    , HH.p_ [ HH.text state.chars ]
    ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    document <- H.liftEffect $ document =<< window
    H.subscribe' \sid ->
      eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)

  HandleKey sid ev
    | KE.shiftKey ev -> do
        H.liftEffect $ E.preventDefault $ KE.toEvent ev
        let char = KE.key ev
        when (String.length char == 1) do
          H.modify_ \st -> st { chars = st.chars <> char }

    | KE.key ev == "Enter" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.modify_ _ { chars = "" }
        H.unsubscribe sid

    | otherwise ->
        pure unit
```

In this example we used the `H.subscribe'` function, which passes the `SubscriptionId` to the event source instead of returning it. This is an alternative that lets you keep the ID in the action type instead of the state, which can be more convenient.

We wrote our event source right into our code to handle the `Initialize` action, which registers an event listener on the document and emits `HandleKey` every time a key is pressed.

`eventListenerEventSource` works a little differently from the other event sources. It uses types from the `purescript-web` libraries for working with the DOM to manually construct an event listener:

```
eventListenerEventSource
  :: forall m a
   . MonadAff m
  => EventType
  -> EventTarget
  -> (Event -> Maybe a)
  -> EventSource m a
```

It takes a type of event to listen to (in our case: `keyup`), a target indicating where to listen for events (in our case: the `HTMLDocument` itself), and a callback function that transforms the events that occur into a type that should be emitted (in our case: we emit our `Action` type by capturing the event in the `HandleKey` constructor).

## Wrapping Up

Halogen components use the `Action` type to handle various kinds of events that arise internally in a component. We've now seen all the common ways this can happen:

1. User interaction with HTML elements we rendered
2. Lifecycle events
3. Event sources, whether via `Aff` and `Effect` functions or from event listeners on the DOM

You now know all the essentials for using Halogen components in isolation. In the next chapter we'll learn how to combine Halogen components together into a tree of parent and child components.