# Handling effects

## Using `Effect` during `eval`

### The Problem

Let's say we have a the following fragment of code in our Halogen component.
```purescript
type State = Int

initialState :: State
initialState = 0

render :: State -> H.ComponentHTML Query
render currentNumber =
  HH.button_
  [ HH.text "Click me!" ]
```

We want this component to be like a counter: every time the button gets clicked, we log the current number to the console and then add 1 to the state. We already know how to update the state, but the corresponding function, `log` from `Effect.Console`, has the type signature: `log :: String -> Effect Unit`. Since we cannot handle Effects in the `render` function, where, then, can we put them?

Halogen components have no built-in mechanisms for dealing with effects during query evaluation. That doesn't mean that they _can't_ have effects, only that there is no implicit mechanism for them. They're made explicit in the usual way: via the type signature.

Let's take another look at the type of `component` from the last chapter:

``` purescript
component :: forall m. H.Component HH.HTML Query Input Message m
```

The `m` parameter we left polymorphic here is our means of introducing effect handling into a component `eval` function.

### The Solution

To be able to use `log` or other effects, we'll populate the `m` type variable with an `Aff`. We need to do this to both the `component` and `eval` function's type signatures:

``` purescript
{-
component :: forall m. H.Component HH.HTML Query Input Message m    -}
component ::           H.Component HH.HTML Query Input Message Aff

{-
eval :: Query a -> H.ComponentDSL State Query Void m a
eval :: Query   ~> H.ComponentDSL State Query Void m    -}
eval :: Query   ~> H.ComponentDSL State Query Void Aff
```

Why are we using `Aff` rather than `Effect`? Mainly for convenience (and so we do not have to explain something that will be explained in the next page). `Aff` can do anything `Effect` can, so we're not losing out, just admitting more possibilities than we might need.

We can now use the [`liftEffect`][Effect.Class.liftEffect] function in `eval`:
```purescript
render :: State -> H.ComponentHTML Query
render currentNumber =
  HH.button
   [ HE.onClick (HE.input_ ShowAndInc) ]
   [ HH.text "Click me!" ]

eval :: Query   ~> H.ComponentDSL State Query Void Aff
eval = case _ of
  ShowAndInc a -> do
    count <- H.get
    H.liftEffect log $ "Count #: " <> show count
    H.put $ count + 1
    pure next
```

This works as there's a [`MonadEffect`][Effect.Class.MonadEffect] instance for `HalogenM` for any `m` that also has a `MonadEffect` instance, and `Aff` satisfies this constraint. (As a reminder, `ComponentDSL` is an synonym for `HalogenM`).

#### Example: Logging to the Console

Here's the full solution. It may be useful to log things to the console when building a component and testing out ideas:

``` purescript
import Prelude
import Effect.Aff (Aff)
import Effect.Console (log)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Number

data Query a = ShowAndInc a

component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 0

  render :: State -> H.ComponentHTML Query
  render state =
    HH.button
      [ HE.onClick (HE.input_ ShowAndInc) ]
      [ HH.text "Click to log the current number to the Console and increment it" ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    ShowAndInc a -> do
      count <- H.get
      H.liftEffect log $ "Count #: " <> show count
      H.put $ count + 1
      pure next
```

#### Example: Generating Random Numbers

Here's a component that generates a random number on demand using [`random`][Effect.Random.random] and displays it to the user:

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

## Recursive Query Evaluation

Returning to event handling, sometimes we want to call `preventDefault` or `stopPropagation` on events to prevent them from interfering with other event handlers. Here are the type signatures of those methods:
```purescript
stopPropagation          :: Event -> Effect Unit
stopImmediatePropagation :: Event -> Effect Unit
preventDefault           :: Event -> Effect Unit
```

Sometimes, that may be all that we want to do. In such cases, we can convert them into a Query type and lift these effects using the methdology described above.

In other cases, we may wish to stop an event's propagation, but still do something with it. This presents a problem that can be solved by a simple trick: recursive query evaluation. Here's the pattern to follow (as [described by cryogenian](cryogenian-solution) and [made somewhat easier/clearer by thomashoneyman](thomashoneyman-cleanup), which I've slightly updated and adapted here):
- In addition to your normal data constuctors for `Query a`, define two additional kinds:
    - One kind that halts a recursive query evaluation called `NoOp a` (e.g. No Operation)
    - A recursive kind that calls an effectful function (e.g. `preventDefault`) before evaluating the next query. It's `a` type should be replaced with `(Query a)`: `EffectfulFunction Arg1 Arg2 {- ArgN -} (Query a)`. For our example, we'll define `PreventDefault Event (Query a)`.
- When evaluating a recursive `Query`, replace the final `pure next` line with `eval query` instead.
- Create the helper function, `recursiveQuery` (implementation shown below), and use it when needed.

Here's the code:
```purescript
module Component where
import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault, stopPropagation, stopImmediatePropagation)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent, clientX)

type State = Unit

type Input = Unit
type Message = Void

data Query a
  -- actual action...
  = LogToConsole String a

  -- Equivalent of "Halt" when there's nothing to log
  | NoOperation a

  -- Recursive queries that can be used to call these functions
  | PreventDefault Event (Query a)
  | StopPropagation Event (Query a)
  | StopImmediatePropagation Event (Query a)

component :: H.Component HH.HTML Query Input Message Aff
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where

  recursiveQuery onEvent eToRecQuery =
    onEvent (\e -> Just $ eToRecQuery e)

  render :: State -> H.ComponentHTML Query
  render _ =
    HH.div
      [ HP.id_ "div1"
      , recursiveQuery HE.onClick (\e ->
          PreventDefault (toEvent e) $
          H.action $ NoOperation
        )
      , HE.onMouseMove (HE.input_ $ LogToConsole "Div1: This will never appear")
      ]
      [ HH.div
        [ HP.id_ "div2"
        , recursiveQuery HE.onClick (\e ->
            let
              event = toEvent e
            in
              PreventDefault event $
              StopPropagation event $
              H.action $ LogToConsole "Either Button or Div 2 was clicked"
          )
        , HE.onMouseMove (HE.input_ $ LogToConsole "Div2: This will never appear")
        ]
        [ HH.button
          [
          -- recursiveQuery HE.onClick (\e -> H.action $ LogToConsole "message" )
          -- ,
            -- recursiveQuery HE.onClick (\e ->
            --   PreventDefault (toEvent e) $
            --   H.action $ LogToConsole $ "Button clicked - ClientX: x" <> (show $ clientX e)
            -- )
          -- ,
            -- recursiveQuery HE.onMouseMove (\mouseEvent ->
            --   let
            --     event = toEvent mouseEvent
            --   in
            --     PreventDefault event $
            --     StopPropagation event $
            --     StopImmediatePropagation event $
            --     H.action $ LogToConsole $
            --       "preventDefault and stopPropagation and stopImmediatePropagation" <>
            --       "were called on Button's mouse move event"
            -- )
          ]
          [ HH.text "Click me and check the console." ]
        ]
      ]


  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    LogToConsole message next -> do
      liftEffect $ log message
      pure next
    NoOperation next -> pure next

    PreventDefault e query -> do
      liftEffect $ preventDefault e

      -- "eval query" will block until the query finishes its evaluation
      -- So, no need to worry about concurrency issues by doing this.
      -- However, beware of recursive
      eval query
    StopPropagation e query -> do
      liftEffect $ stopPropagation e
      eval query
    StopImmediatePropagation e query -> do
      liftEffect $ stopImmediatePropagation e
      eval query
```
## Conclusion

We've shown you
- how to use `Effect`-based and `Aff`-based effects in your code
- how to do recursive query evaluation

Let's take a look at [running a component][running-components] to produce a UI next.

[purescript-affjax]: https://pursuit.purescript.org/packages/purescript-affjax "purescript-affjax"

[Effect.Aff.Class.liftAff]: https://pursuit.purescript.org/packages/purescript-aff/4.0.0/docs/Effect.Aff.Class#v:liftAff "Effect.Aff.Class.liftAff"
[Effect.Aff.Class.MonadAff]: https://pursuit.purescript.org/packages/purescript-aff/4.0.0/docs/Effect.Aff.Class#t:MonadAff "Effect.Aff.Class.MonadAff"
[Effect.Class.liftEffect]: https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Class#v:liftEffect "Effect.Class.liftEffect"
[Effect.Class.MonadEffect]: https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Class#t:MonadEffect "Effect.Class.MonadEffect"
[Effect.Random.random]: https://pursuit.purescript.org/packages/purescript-random/4.0.0/docs/Effect.Random#v:random "Effect.Random.random"
[Halogen.Component.hoist]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component#v:hoist "Halogen.Component.hoist"

[cryogenian-solution]: https://github.com/slamdata/purescript-halogen/issues/426#issuecomment-284743053
[thomashoneyman-cleanup]: https://github.com/slamdata/purescript-halogen/issues/426#issuecomment-320390523

[running-components]: 4%20-%20Running%20a%20component.md "Running a component"
