# Performing Effects

We've covered a lot of ground so far. You know how to write Halogen HTML. You can define components that respond to user interactions and model each part of the component in types. With this foundation we can move on to another vital tool when writing applications: performing effects.

In this chapter we'll explore how to perform effects in your component through two examples: generating random numbers and making HTTP requests. Once you know how to perform effects you are well on your way to mastering Halogen fundamentals.

Before we start, it's important to know that you can only perform effects during evaluation, which means functions like `handleAction` which use the type `HalogenM`. You can't perform effects when you produce your initial state or during rendering. Since you can only perform effects when you're within `HalogenM`, let's briefly learn more about it before diving in to the examples.

## The `HalogenM` Type

If you recall from last chapter, the `handleAction` function returns a type called `HalogenM`. Here's the `handleAction` we wrote:

```purs
handleAction :: forall output m. Action -> HalogenM State Action () output m Unit
```

`HalogenM` is a crucial part of Halogen, often called the "eval" monad. This monad enables Halogen features like state, forking threads, starting subscriptions, and more. But it's quite limited, concerning itself only with Halogen-specific features. In fact, Halogen components have no built-in mechanisms for effects!

Instead, Halogen lets you choose what monad you would like to use with `HalogenM` in your component. You gain access to all the capabilities of `HalogenM` _and also_ whatever capabilities your chosen monad supports. This is represented with the type parameter `m`, which stands for "monad".

A component that only uses Halogen-specific features can leave this type parameter open. Our counter, for example, only updated state. But a component that performs effects can use the `Effect` or `Aff` monads, or you can supply a custom monad of your own.

This `handleAction` is able to use functions from `HalogenM` like `modify_` and can also use effectful functions from `Effect`:

```purs
handleAction :: forall output. Action -> HalogenM State Action () output Effect Unit
```

This one can use functions from `HalogenM` and also effectful functions from `Aff`:

```purs
handleAction :: forall output. Action -> HalogenM State Action () output Aff Unit
```

It is more common in Halogen to use constraints on the type parameter `m` to describe what the monad can do rather than choose a specific monad, which allows you to mix several monads together as your application grows. For example, most Halogen apps would use functions from `Aff` via this type signature:

```purs
handleAction :: forall output m. MonadAff m => Action -> HalogenM State Action () output m Unit
```

This lets you do everything the hardcoded `Aff` type did, but it also lets you mix in other constraints too.

One last thing: when you choose a monad for your component it will show up in your `HalogenM` type, your `Component` type, and, if you are using child components, in your `ComponentHTML` type:

```purs
component :: forall query input output m. MonadAff m => H.Component query input output m

handleAction :: forall output m. MonadAff m => Action -> HalogenM State Action () output m Unit

-- We aren't using child components, so we don't have to use the constraint here, but
-- we'll learn about when it's required in the parent & child components chapter.
render :: forall m. State -> H.ComponentHTML Action () m
```

## An `Effect` Example: Random Numbers

Let's create a new, simple component that generates a new random number each time you click a button. As you read through the example, notice how it uses the same types and functions that we used to write our counter. Over time you'll become used to scanning the state, action, and other types of a Halogen component to get a gist of what it does, and familiar with standard functions like `initialState`, `render`, and `handleAction`.

> You can paste this example into [Try Purescript](https://try.purescript.org) to explore it interactively. You can also see and run the [full example code](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples/effects-effect-random) from the `examples` directory in this repository.

Notice that we don't perform any effects in our `initialState` or `render` functions -- for example, we initialize our state to `Nothing` rather than generate a random number for our initial state -- but we're free to perform effects in our `handleAction` function (which uses the `HalogenM` type).

```purs
module Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State = Maybe Number

data Action = Regenerate

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
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
        [ HE.onClick \_ -> Regenerate ]
        [ HH.text "Generate new number" ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.modify_ \_ -> Just newNumber
```

As you can see, a component that performs effects is not much different from a component that doesn't! We've only done two things:

1. We added a `MonadEffect` constraint to the `m` type parameter for our component _and_ for our `handleAction` function. We don't need the constraint for our render function because we don't have any child components.
2. We actually _used_ an effect for the first time: the `random` function, which comes from `Effect.Random`.

Let's break down using this effect a little more.

```purs
--                          [1]
handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random -- [2]
    H.modify_ \_ -> Just newNumber   -- [3]
```

1. We have constrained our `m` type parameter to say we support any monad, so long as that monad supports `MonadEffect`. It's another way to say "We need to be able to use `Effect` functions in our evaluation code."
2. The `random` function has the type `Effect Number`. But we can't use it directly: our component doesn't support `Effect` but rather _any_ monad `m` so long as that monad can run effects from `Effect`. It's a subtle difference, but in the end we require the `random` function to have the type `MonadEffect m => m Number` instead of being `Effect` directly. Fortunately, we can convert any `Effect` type to `MonadEffect m => m` using the `liftEffect` function. This is a common pattern in Halogen, so keep `liftEffect` in mind if you're using `MonadEffect`.
3. The `modify_` function lets you update state, and it comes directly from `HalogenM` with the other state update functions. Here we use it to write the new random number to our state.

This is a nice example of how you can freely interleave effects from `Effect` with Halogen-specific functions like `modify_`. Let's do it again, this time using the `Aff` monad for asynchronous effects.

## An `Aff` Example: HTTP Requests

It's common to fetch information from elsewhere on the Internet. For example, let's say we'd like to work with GitHub's API to fetch users. We'll use the [`affjax`](https://pursuit.purescript.org/packages/purescript-affjax) package to make our requests, which itself relies on the `Aff` monad for asynchronous effects.

This example is even more interesting, though: we'll also use the `preventDefault` function to prevent form submission from refreshing the page, which runs in `Effect`. That means our example shows how you can interleave different effects together (`Effect` and `Aff`) along with Halogen functions (`HalogenM`).

> As with the Random example, you can paste this example into [Try Purescript](https://try.purescript.org) to explore it interactively. You can also see and run the [full example code](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples/effects-aff-ajax) from the `examples` directory in this repository.

This component definition should start to look familiar. We define our `State` and `Action` types and implement our `initialState`, `render`, and `handleAction` functions. We bring them together into our component spec and turn them into a valid component `H.mkComponent`.

Once again, notice that our effects are concentrated in the `handleAction` function and no effects are performed when making the initial state or rendering Halogen HTML.

```purs
module Main where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Web.Event.Event as Event

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = { loading: false, username: "", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit \ev -> MakeRequest ev ]
    [ HH.h1_ [ HH.text "Look up GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput \str -> SetUsername str
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text $ if st.loading then "Working..." else "" ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ _ { username = username, result = Nothing }

  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ _ { loading = true }
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ _ { loading = false, result = map _.body (hush response) }
```

This example is especially interesting because:

1. It mixes together functions from multiple monads (`preventDefault` is `Effect`, `AX.get` is `Aff`, and `gets` and `modify_` are `HalogenM`). We're able to use `liftEffect` and `liftAff` along with our constraints to make sure everything plays well together.
2. We only have one constraint, `MonadAff`. That's because anything that can be run in `Effect` can also be run in `Aff`, so `MonadAff` implies `MonadEffect`.
3. We're making multiple state updates in one evaluation.

That last point is especially important: when you modify state your component renders. That means that during this evaluation we:

1. Set `loading` to `true`, which causes the component to re-render and display "Working..."
2. Set `loading` to `false` and update the result, which causes the component to re-render and display the result (if there was one).

It's worth noting that because we're using `MonadAff` our request will not block the component from doing other work, and we don't have to deal with callbacks to get this async superpower. The computation we've written in `MakeRequest` simply suspends until we get the response and then proceeds to update the state the second time.

It's a smart idea to only modify state when necessary and to batch updates together if possible (like how we call `modify_` once to update both the `loading` and `result` fields). That helps make sure you're only re-rendering when needed.

### Event Handling Revisited

There is a lot going on in this example, so it is worth concentrating for a moment on the new event-handling features which it introduces.  Unlike the simple click handlers of the button example, the handlers defined here do make use of the event data they are given:

- The value of the username input is used by the `onValueInput` handler (the `SetUsername` action).
- `preventDefault` is called on the event in the `onSubmit` handler (the `MakeRequest` action).

The type of parameter passed to the handler depends on which function is used to attach it. Sometimes, as for `onValueInput`, the handler simply receives data extracted from the event - a `String` in this case. Most of the other `on...` functions set up a handler to receive the whole event, either as a value of type `Event`, or as a specialised type like `MouseEvent`. The details can be found in the [module documentation for `Halogen.HTML.Events`](https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.HTML.Events) on pursuit; the types and functions used for events can be found in the [web-events](https://pursuit.purescript.org/packages/purescript-web-events) and [web-uievents](https://pursuit.purescript.org/packages/purescript-web-uievents) packages.
