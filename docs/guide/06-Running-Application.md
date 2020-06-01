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
component :: forall q i o m. H.Component q i o m
component = ...
```

The most important function used in `main` is the `runUI` function. Provide `runUI` with your root component, the root component's input value, and a reference to a DOM element, and it will provide your application to the Halogen virtual DOM. The virtual DOM will then render your application at that element and maintain it there for as long as your app is running.

```purs
runUI
  :: forall query input output
   . Component HTML query input output Aff
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

When you run your Halogen application with `runUI` you receive a record of functions with the type `DriverIO`. These functions can be used to control your root component from outside the application. Conceptually, they're like a makeshift parent component for your application.

```purs
type HalogenIO query output m =
  { query :: forall a. query a -> m (Maybe a)
  , subscribe :: Control.Coroutine.Consumer output m Unit -> m Unit
  , dispose :: m Unit
  }
```

1. The `query` function should look similar to you -- it's like the ordinary `H.query` function we used for parent components to imperatively tell a child component to do something or to request some information from it.
2. The `subscribe` function can be used to subscribe to a stream of output messages from the component -- it's like the handler we provided to the `slot` function, except rather than evaluate an action here we can perform some effect instead.
3. The `dispose` function can be used to halt and clean up the Halogen application. This will kill any forked threads, close all subscriptions, and so on.

A common pattern in Halogen applications is to use a `Route` component as the root of the application, and use the `query` function from `HalogenIO` to trigger route changes in the application when the URL changes. You can see a full example of doing this in the [Real World Halogen `Main.purs` file](https://github.com/thomashoneyman/purescript-halogen-realworld/blob/master/src/Main.purs).

## Full Example: Controlling a Button With `HalogenIO`

You can paste this example into [Try PureScript](https://try.purescript.org) to explore using `HalogenIO` to control the root component of an application.

```purs
module Main where

import Prelude

import Control.Coroutine as CR
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body
  
  -- Log a message from outside the application by sending it to the button
  let logMessage str = void $ io.query $ H.tell $ AppendMessage str

  io.subscribe $ CR.consumer \(Toggled newState) -> do
    logMessage $ "Button was internally toggled to: " <> show newState
    pure Nothing

  state0 <- io.query $ H.request IsOn
  logMessage $ "The button state is currently: " <> show state0

  _ <- io.query $ H.tell $ SetEnabled true

  state1 <- io.query $ H.request IsOn
  logMessage $ "The button state is now: " <> show state1

-- Child component implementation

data Query a
  = IsOn (Boolean -> a)
  | SetEnabled Boolean a
  | AppendMessage String a

data Output = Toggled Boolean

data Action = Toggle

type State = { enabled :: Boolean, messages :: Array String }

component :: forall i m. H.Component HH.HTML Query i Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }
  where
  initialState :: i -> State
  initialState _ = { enabled: false, messages: [] }

  render :: State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.div_ (map (\str -> HH.p_ [ HH.text str ]) state.messages)
      , HH.button
          [ HE.onClick \_ -> Just Toggle ]
          [ HH.text $ if state.enabled then "On" else "Off" ]
      ]
  
  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Toggle -> do
      newState <- H.modify \st -> st { enabled = not st.enabled }
      H.raise (Toggled newState.enabled)
  
  handleQuery :: forall a. Query a -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    IsOn reply -> do
      enabled <- H.gets _.enabled
      pure (Just (reply enabled))
      
    SetEnabled enabled a -> do
      H.modify_ _ { enabled = enabled }
      pure (Just a)
    
    AppendMessage str a -> do
      H.modify_ \st -> st { messages = Array.snoc st.messages str }
      pure (Just a)
```