# Running an Application

Over the course of this guide we've seen the standard way to run a Halogen application several times. In this chapter, we'll learn what is actually going on when we run a Halogen application and how to control a running app from the outside.

## Using `runUI` and `awaitBody`

PureScript applications use the `main` function in their `Main` module as their entrypoint. Here's a standard `main` function for Halogen apps:

```purs
module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

-- Assuming you have defined a root component for your application
component :: forall query input output m. H.Component query input output m
component = ...
```

The most important function used in `main` is the `runUI` function. Provide `runUI` with your root component, the root component's input value, and a reference to a DOM element, and it will provide your application to the Halogen virtual DOM. The virtual DOM will then render your application at that element and maintain it there for as long as your app is running.

```purs
runUI
  :: forall query input output
   . Component query input output Aff
  -> input
  -> DOM.HTMLElement
  -> Aff (HalogenIO query output Aff)
```

As you can see, the `runUI` function requires that your Halogen application can ultimately be run in the `Aff` monad. In this guide we used constraints like `MonadEffect` and `MonadAff`, which `Aff` satisfies, so we're in the clear.

> If you chose to use another monad for your application then you'll need to hoist it to run in `Aff` before you provide your application to `runUI`. The [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) uses a custom `AppM` monad that serves as a good example of how to do this.

In addition to `runUI` we used two other helper functions. First, we used `awaitBody` to wait for the page to load and then acquire a reference to the `<body>` tag as the root HTML element for the application to control. Second, we used `runHalogenAff` to launch asynchronous effects (our `Aff` code containing `awaitBody` and `runUI`) from within `Effect`. This is necessary because `awaitBody`, `runUI`, and our applications run in the `Aff` monad, but PureScript `main` functions must be in `Effect`.

The `main` function we've used here is the standard way to run a Halogen application that is the only thing running on the page. Sometimes, though, you may use Halogen to take over just one part of the page, or you may be running multiple Halogen apps. In these cases, you'll probably reach for a pair of different helper functions:

1. `awaitLoad` blocks until the document has loaded so that you can safely retrieve references to HTML elements on the page
2. `selectElement` can be used to target a particular element on the page to embed the app within

## Using `HalogenIO`

When you run your Halogen application with `runUI` you receive a record of functions with the type `HalogenIO`. These functions can be used to control your root component from outside the application. Conceptually, they're like a makeshift parent component for your application.

```purs
type HalogenIO query output m =
  { query :: forall a. query a -> m (Maybe a)
  , messages :: Event output
  , dispose :: m Unit
  }
```

1. The `query` function is like the `H.query` function which underpins `tell` and `request`. This allows you to send queries to the root component of your application from outside the application.
2. The `messages` event can be used to subscribe to a stream of output messages from the component -- it's like the handler we provided to the `slot` function, except rather than evaluate an action here we can perform some effect instead.
3. The `dispose` function can be used to halt and clean up the Halogen application. This will kill any forked threads, close all subscriptions, and so on.

You can't use `tell` and `request` at the root of your application, but you can use the `mkTell` and `mkRequest` functions (as seen in the example below) for a similar effect.

A common pattern in Halogen applications is to use a `Route` component as the root of the application, and use the `query` function from `HalogenIO` to trigger route changes in the application when the URL changes. You can see a full example of doing this in the [Real World Halogen `Main.purs` file](https://github.com/thomashoneyman/purescript-halogen-realworld/blob/master/src/Main.purs).

## Full Example: Controlling a Button With `HalogenIO`

You can paste this example into [Try PureScript](https://try.purescript.org) to explore using `HalogenIO` to control the root component of an application.

```purs
module Example.Driver.IO.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Example.Driver.IO.Button as B
import FRP.Event as Event
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.component unit body

  _ <- liftEffect $ Event.subscribe io.messages \(Toggled newState) -> do
    liftEffect $ log $ "Button was internally toggled to: " <> show newState
    pure Nothing

  state0 <- io.query $ H.mkRequest IsOn
  liftEffect $ log $ "The button state is currently: " <> show state0

  void $ io.query $ H.mkTell (B.SetState true)

  state1 <- io.query $ H.mkRequest IsOn
  liftEffect $ log $ "The button state is now: " <> show state1

-- Child component implementation

type Slot = H.Slot Query Message

data Query a
  = IsOn (Boolean -> a)
  | SetState Boolean a

data Message = Toggled Boolean

data Action = Toggle

type State = { enabled :: Boolean }

component :: forall i m. H.Component Query i Message m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.button
      [ HP.title label
      , HE.onClick \_ -> Just Toggle
      ]
      [ HH.text label ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction = case _ of
  Toggle -> do
    newState <- H.modify \st -> st { enabled = not st.enabled }
    H.raise (Toggled newState.enabled)

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  IsOn k -> do
    enabled <- H.gets _.enabled
    pure (Just (k enabled))
  SetState enabled a -> do
    H.modify_ (_ { enabled = enabled })
    pure (Just a)
```
