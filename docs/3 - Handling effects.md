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
import Prelude
import Effect.Aff (Aff)
import Effect.Random (random)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Maybe Number

data Query a = Regenerate a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Nothing

  render :: State -> H.ComponentHTML Query
  render state =
    let
      value = maybe "No number generated yet" show state
    in
      HH.div_ $
        [ HH.h1_ [ HH.text "Random number" ]
        , HH.p_ [ HH.text ("Current value: " <> value) ]
        , HH.button
            [ HE.onClick (HE.input_ Regenerate) ]
            [ HH.text "Generate new number" ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    Regenerate next -> do
      newNumber <- H.liftEffect random
      H.put (Just newNumber)
      pure next
```

A runnable version of this is available in the [`effects-eff-random` example](../examples/effects-eff-random/).

To be able to use [`random`][Effect.Random.random], we've populated the `m` type variable with an `Aff`. This needs applying to both the component and `eval` function:

``` purescript
ui :: H.Component HH.HTML Query Unit Void Aff

eval :: Query ~> H.ComponentDSL State Query Void Aff
```

Why are we using `Aff` rather than `Effect`? For convenience - when it's time to run our UI, Halogen expects an `Aff` here. It is possible to [`hoist`][Halogen.Component.hoist] a component and change the `m` type, but it's easier if we just use `Aff` in the first place. `Aff` can do anything `Effect` can, so we're not losing out, just admitting more possibilities than we might need.

We can now use the [`liftEffect`][Effect.Class.liftEffect] function in `eval`:

``` purescript
eval = case _ of
  Regenerate next -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)
    pure next
```

This works as there's a [`MonadEffect`][Effect.Class.MonadEffect] instance for `HalogenM` for any `m` that also has a `MonadEffect` instance, and `Aff` satisfies this constraint. (As a reminder, `ComponentDSL` is an synonym for `HalogenM`).

## Using `Aff` during `eval`

Occasionally it's useful to be able to fetch data from an API, so let's use that for the next example. We're going to make use of the [`affjax`][purescript-affjax] library as it provides a nice `Aff`-based interface for AJAX requests. Our data source will be GitHub's user API.

``` purescript
import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Network.HTTP.Affjax.Response as AXResponse

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, username: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ HH.h1_ [ HH.text "Lookup GitHub user" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
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

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    SetUsername username next -> do
      H.modify_ (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXResponse.string ("https://api.github.com/users/" <> username)
      H.modify_ (_ { loading = false, result = Just response.response })
      pure next
```

A runnable version of this is available in the [`effects-aff-ajax` example](../examples/effects-aff-ajax/).

As with the `Effect`-based example, we've populated the `m` type variables with `Aff`. This time we're going to rely on the [`MonadAff`][Effect.Aff.Class.MonadAff] instance and use [`liftAff`][Effect.Aff.Class.liftAff]:

``` purescript
MakeRequest next -> do
  username <- H.gets _.username
  H.modify_ (_ { loading = true })
  response <- H.liftAff $ AX.get AXResponse.string ("https://api.github.com/users/" <> username)
  H.modify_ (_ { loading = false, result = Just response.response })
  pure next
```

Note how there was no need to setup callbacks or anything of that nature. Using `liftAff` means we can mix the behaviour of `Aff` with our other component-related operations, giving us seamless async capabilities.

## Mixing `Effect` and `Aff`

Any type that satisfies a `MonadAff` constraint also satisfies `MonadEffect`, so using `Aff` as the base monad for a component allows `liftEffect` and `liftAff` to be used together freely.

Let's take a look at [running a component][running-components] to produce a UI next.

[purescript-affjax]: https://pursuit.purescript.org/packages/purescript-affjax "purescript-affjax"

[Effect.Aff.Class.liftAff]: https://pursuit.purescript.org/packages/purescript-aff/4.0.0/docs/Effect.Aff.Class#v:liftAff "Effect.Aff.Class.liftAff"
[Effect.Aff.Class.MonadAff]: https://pursuit.purescript.org/packages/purescript-aff/4.0.0/docs/Effect.Aff.Class#t:MonadAff "Effect.Aff.Class.MonadAff"
[Effect.Class.liftEffect]: https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Class#v:liftEffect "Effect.Class.liftEffect"
[Effect.Class.MonadEffect]: https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Class#t:MonadEffect "Effect.Class.MonadEffect"
[Effect.Random.random]: https://pursuit.purescript.org/packages/purescript-random/4.0.0/docs/Effect.Random#v:random "Effect.Random.random"
[Halogen.Component.hoist]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component#v:hoist "Halogen.Component.hoist"

[running-components]: 4%20-%20Running%20a%20component.md "Running a component"
