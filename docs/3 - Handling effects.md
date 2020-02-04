# Handling effects

Halogen components have no built-in mechanisms for dealing with effects during query evaluation. That doesn't mean that they _can't_ have effects, only that there is no implicit mechanism for them. They're made explicit in the usual way: via the type signature.

Let's take another look at the type of the button component from the last chapter:

``` purescript
myButton :: forall m. H.Component HH.HTML Query Input Message m
```

The `m` parameter we left polymorphic here is our means of introducing effect handling into a component `eval` function.

## Using `Effect` during `eval`

Here's a component that generates a random number on demand and displays it to the user:

``` purescript
module Example.Effects.Eff.Random.Component where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = Regenerate

type State = Maybe Number

component :: forall f i o m. MonadEffect m => H.Component HH.HTML f i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    value = maybe "No number generated yet" show state
  in
    HH.div_ $
      [ HH.h1_ [ HH.text "Random number" ]
      , HH.p_ [ HH.text ("Current value: " <> value) ]
      , HH.button
          [ HE.onClick \_ -> Just Regenerate ]
          [ HH.text "Generate new number" ]
      ]

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)

```

A runnable version of this is available in the [`effects-eff-random` example](../examples/effects-eff-random/).

To be able to use [`random`][Effect.Random.random], we've constrained `m` to have an instance of `MonadEffect`, so we can now run this component in `Effect`, or `Aff` or any other monad which implements `MonadEffect`.

``` purescript
component :: forall f i o m. MonadEffect m => H.Component HH.HTML f i o m

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
```

Why are we using a constraint of `MonadEffect` rather than `Effect` or `Aff`? For convenience - this component is now polymorphic over any monad that supports effects. So our component is easily re-used across multiple applications that might be using different monads to run Halogen. It is possible to [`hoist`][Halogen.Component.hoist] a component, allowing us to change the `m` type, and this polymorphic constraint allows us to keep our options open as our application design changes.

For a detailed explanation of `hoist`, please refer to the chapter on running Halogen components on the DOM.

We can now use the [`liftEffect`][Effect.Class.liftEffect] function in `handleAction`:

``` purescript
handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)
```

This works as there's a [`MonadEffect`][Effect.Class.MonadEffect] instance for `HalogenM` for any `m` that also has a `MonadEffect` instance.

## Using `Aff` during `eval`

Occasionally it's useful to be able to fetch data from an API, so let's use that for the next example. We're going to make use of the [`affjax`][purescript-affjax] library as it provides a nice `Aff`-based interface for AJAX requests. Our data source will be GitHub's user API.

``` purescript
module Example.Effects.Aff.Ajax.Component where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Action
  = SetUsername String
  | MakeRequest Event

component :: forall f i o m. MonadAff m => H.Component HH.HTML f i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { loading: false, username: "", result: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.form
    [ HE.onSubmit (Just <<< MakeRequest) ]
    [ HH.h1_ [ HH.text "Lookup GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput (Just <<< SetUsername)
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HP.type_ HP.ButtonSubmit
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
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

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  SetUsername username -> do
    H.modify_ (_ { username = username, result = Nothing :: Maybe String })
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ (_ { loading = false, result = hush response.body })
```

A runnable version of this is available in the [`effects-aff-ajax` example](../examples/effects-aff-ajax/).

As with the `Effect`-based example, we have constrained our component to use `MonadAff`, which subsumes synchronous effects while permitting asynchronous effects. `HalogenM` also has a [`MonadAff`][Effect.Aff.Class.MonadAff] instance where `m` has a `MonadAff` instance, and so we can now use [`liftAff`][Effect.Aff.Class.liftAff] in our `handleAff`:

``` purescript
  MakeRequest event -> do
    H.liftEffect $ Event.preventDefault event
    username <- H.gets _.username
    H.modify_ (_ { loading = true })
    response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
    H.modify_ (_ { loading = false, result = hush response.body })
```

Note how there was no need to setup callbacks or anything of that nature. Using `liftAff` means we can mix the behaviour of `Aff` with our other component-related operations, giving us seamless async capabilities.

## Mixing `Effect` and `Aff`

Any type that satisfies a `MonadAff` constraint also satisfies `MonadEffect`, so using `Aff` as the base monad for a component allows `liftEffect` and `liftAff` to be used together freely.

Let's take a look at [running a component][running-components] to produce a UI next, where we'll also cover how to mount a component in `Effect` or `Aff`.

[purescript-affjax]: https://pursuit.purescript.org/packages/purescript-affjax "purescript-affjax"

[Effect.Aff.Class.liftAff]: https://pursuit.purescript.org/packages/purescript-aff/4.0.0/docs/Effect.Aff.Class#v:liftAff "Effect.Aff.Class.liftAff"
[Effect.Aff.Class.MonadAff]: https://pursuit.purescript.org/packages/purescript-aff/4.0.0/docs/Effect.Aff.Class#t:MonadAff "Effect.Aff.Class.MonadAff"
[Effect.Class.liftEffect]: https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Class#v:liftEffect "Effect.Class.liftEffect"
[Effect.Class.MonadEffect]: https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Class#t:MonadEffect "Effect.Class.MonadEffect"
[Effect.Random.random]: https://pursuit.purescript.org/packages/purescript-random/4.0.0/docs/Effect.Random#v:random "Effect.Random.random"
[Halogen.Component.hoist]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component#v:hoist "Halogen.Component.hoist"

[running-components]: 4%20-%20Running%20a%20component.md "Running a component"
