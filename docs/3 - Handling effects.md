# Handling effects

Halogen components have no built-in mechanisms for dealing with effects during query evaluation. That doesn't mean that they _can't_ have effects, only that there is no implicit mechanism for them. They're made explicit in the usual way: via the type signature.

Let's take another look at the type of the button component from the last chapter:

``` purescript
myButton :: forall m. H.Component HH.HTML Query Input Message m
```

The `m` parameter we left polymorphic here is our means of introducing effect handling into a component `eval` function.

## Using `Eff` during `eval`

Here's a component that generates a random number on demand and displays it to the user:

``` purescript
import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Maybe Number

data Query a = Regenerate a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (random :: RANDOM | eff))
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
  eval = case _ of
    Regenerate next -> do
      newNumber <- H.liftEff random
      H.put (Just newNumber)
      pure next
```

A runnable version of this is available in the [`effects-eff-random` example](../examples/effects-eff-random/).

To be able to use [`random`][Control.Monad.Eff.Random.random], we've populated the `m` type variable with an `Aff`. This needs applying to both the component and `eval` function:

``` purescript
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (random :: RANDOM | eff))

eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
```

Why are we using `Aff` rather than `Eff`? For convenience - when it's time to run our UI, Halogen expects an `Aff` here. It is possible to [`hoist`][Halogen.Component.hoist] a component and change the `m` type, but it's easier if we just use `Aff` in the first place. `Aff` can do anything `Eff` can, so we're not losing out, just admitting more possibilities than we might need.

We can now use the [`liftEff`][Control.Monad.Eff.Class.liftEff] function in `eval`:

``` purescript
eval = case _ of
  Regenerate next -> do
    newNumber <- H.liftEff random
    H.put (Just newNumber)
    pure next
```

This works as there's a [`MonadEff`][Control.Monad.Eff.Class.MonadEff] instance for `HalogenM` for any `m` that also has a `MonadEff` instance, and `Aff` satisfies this constraint. (As a reminder, `ComponentDSL` is an synonym for `HalogenM`).

## Using `Aff` during `eval`

Occasionally it's useful to be able to fetch data from an API, so let's use that for the next example. We're going to make use of the [`affjax`][purescript-affjax] library as it provides a nice `Aff`-based interface for AJAX requests. Our data source will be GitHub's user API.

``` purescript
import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
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
    HH.div_ $
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
  eval = case _ of
    SetUsername username next -> do
      H.modify (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get ("https://api.github.com/users/" <> username)
      H.modify (_ { loading = false, result = Just response.response })
      pure next

```

A runnable version of this is available in the [`effects-aff-ajax` example](../examples/effects-aff-ajax/).

As with the `Eff`-based example, we've populated the `m` type variables with `Aff`. This time we're going to rely on the [`MonadAff`][Control.Monad.Aff.Class.MonadAff] instance and use [`liftAff`][Control.Monad.Aff.Class.liftAff]:

``` purescript
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get ("https://api.github.com/users/" <> username)
      H.modify (_ { loading = false, result = Just response.response })
      pure next
```

Note how there was no need to setup callbacks or anything of that nature. Using `liftAff` means we can mix the behaviour of `Aff` with our other component-related operations, giving us seamless async capabilities.

## Mixing `Eff` and `Aff`

Any type that satisfies a `MonadAff` constraint also satisfies `MonadEff`, so using `Aff` as the base monad for a component allows `liftEff` and `liftAff` to be used together freely. The effect row will need to contain both sets of effects, but other than that no special handling is required.

Let's take a look at [running a component][running-components] to produce a UI next.

[purescript-affjax]: https://pursuit.purescript.org/packages/purescript-affjax "purescript-affjax"

[Control.Monad.Aff.Class.liftAff]: https://pursuit.purescript.org/packages/purescript-aff/2.0.2/docs/Control.Monad.Aff.Class#v:liftAff "Control.Monad.Aff.Class.liftAff"
[Control.Monad.Aff.Class.MonadAff]: https://pursuit.purescript.org/packages/purescript-aff/2.0.2/docs/Control.Monad.Aff.Class#t:MonadAff "Control.Monad.Aff.Class.MonadAff"
[Control.Monad.Eff.Class.liftEff]: https://pursuit.purescript.org/packages/purescript-eff/2.0.0/docs/Control.Monad.Eff.Class#v:liftEff "Control.Monad.Eff.Class.liftEff"
[Control.Monad.Eff.Class.MonadEff]: https://pursuit.purescript.org/packages/purescript-eff/2.0.0/docs/Control.Monad.Eff.Class#t:MonadEff "Control.Monad.Eff.Class.MonadEff"
[Control.Monad.Eff.Random.random]: https://pursuit.purescript.org/packages/purescript-random/2.0.0/docs/Control.Monad.Eff.Random#v:random "Control.Monad.Eff.Random.random"
[Halogen.Component.hoist]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#v:hoist "Halogen.Component.hoist"

[running-components]: 4%20-%20Running%20a%20component.md "Running a component"
