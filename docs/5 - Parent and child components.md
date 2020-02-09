# Parent and child components

So far the examples have only concerned a single component, however this will only take us so far before the state and query algebra becomes unmanageable. The answer is to break our app into components that can be composed.

Let's take a look at a component that uses our button component as a child:

``` purescript
module Example.Components.Container (component) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Example.Components.Button as Button
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action
  = HandleButton Button.Message
  | CheckButtonState

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

type ChildSlots =
  ( button :: Button.Slot Unit
  )

_button :: SProxy "button"
_button = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { toggleCount: 0
  , buttonState: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.slot _button unit Button.component unit (Just <<< HandleButton)
    , HH.p_
        [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
    , HH.p_
        [ HH.text
            $ "Last time I checked, the button was: "
            <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
            <> ". "
        , HH.button
            [ HE.onClick (\_ -> Just CheckButtonState) ]
            [ HH.text "Check now" ]
        ]
    ]

handleAction ::forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleButton (Button.Toggled _) -> do
    H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })
  CheckButtonState -> do
    buttonState <- H.query _button unit $ H.request Button.IsOn
    H.modify_ (_ { buttonState = buttonState })
```

A runnable version of this is available in the [`components` example](../examples/components/).

This is a somewhat silly example of a container wrapping a button. It counts how many times the button has been toggled, and when asked it can check whether the button is on or off. In reality the "ask" part of this would be unnecessary as the container could use the message from the button to track the state changes, but doing this gives us an excuse to illustrate a request query.

As you can see, things are much the same as with a standalone component, only there are some new types involved.

## Child Slots

The first new element we see defined for this component is the `ChildSlots` type. We use values of this type as the IDs for child components in the rendered HTML. "Slot", "slot address", "slot id" are all used interchangeably to refer to these values.

We also supply a function which uses `SProxy` to access the button component itself.

## Rendering
The render function for a parent component must define the childSlots type it will be rendering, in contrast, previously we have supplied the empty row `()` in this position:

``` purescript
-- Render in general
state -> s a c m

-- Render for a standalone component
render :: forall m. State -> H.ComponentHTML Action () m

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
```

- `s` is the surface or way that we'll be rendering the component (typically HTML).
- `a` is the action type that can be launched from the rendered component.
- `c` is the child slot address type, (`()` in the case where a component has no children).
- `m` is the effect monad the component will run in (Generally this will be polymorphic).

It may seem a little odd that we have to include `m` here when rendering, since no side effects can occur here. We do need evidence that both parent and child components share the same effect monad type for things to work out though.

When we want to render a child component in the HTML DSL we use the [`slot`][Halogen.HTML.slot] function:

``` purescript
slot
  :: forall query action input output slots m label slot _1
     . Row.Cons label (Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> Component HTML query input output m
  -> input
  -> (output -> Maybe action)
  -> ComponentHTML action slots m
```

We pass it:

- `SProxy label` - the accessor function that uses `SProxy` to get the desired child component by the string name we have given it in the `ChildSlots` type.
- `slot` - a unique ID or index value used to track the individual components and their relative positions. This is useful when the same component needs to be replicated and displayed multiple times. In cases where the same component is not being rendered as siblings within the component, you can supply `unit` to this parameter.
- `Component HTML query input output m` - the component being rendered.
- `input` - the input value for that component (or `unit`).
- `(output -> Maybe action)` - a handler function for output messages.

So for our example, that was:

``` purescript
HH.slot _button unit Button.component unit (Just <<< HandleButton)
```

We're using:

- the `_button` function value we created using `SProxy`.
- `unit`, since we're only rendering this component once.
- the button component.
- `unit` for the input value (that's all the button component expects).
- a mapping to the `HandleButton` action for our parent component as the message handler.

The handler function takes a message value from the child component and translates it into an action on the parent component. We can filter the messages by using the `Maybe` return type, so if we're not interested in what the child has to say we can just use `const Nothing`. If the child outputs no messages, using `Void` as its message type, we can use [`absurd`][Data.Void.absurd].

Care should be taken to avoid using the same slot address for multiple child components. The resulting behaviour is undefined... but almost certainly won't be good. If duplicate slot values are detected a warning message will be logged in the browser console.

After a component is initialized in a slot, providing a different value for any parameter other than the input value or slot address will have no effect on the child component. Say we were to render the parent using:

``` purescript
HH.slot _button unit Button.component unit (Just <<< HandleButton)
```

And then on the next render:

``` purescript
HH.slot _button unit SomeOtherButton.component unit (Just <<< HandleButton2)
```

The change in component and handler would have **no effect**. The child component that was initialized in that slot will be preserved, and the original handler setup will be used when it emits messages.

Changing the slot address value will cause the original component to be destroyed and a new one will be initialized in its place with a fresh state.

Changing input values will be covered later in this chapter.

## Querying

The `mkEval` function allows you to supply several functions for each of the possible ways to interact with the component, including thee evaluation of actions, initialization, finalization (unmounting the component), receiving new input, and handling queries. Here we will discuss `handleQuery`, which allows the component to evaluate queries.

An excellent reference can be found in the [`router` example](../examples/driver-routing/):

``` purescript
module Example.Driver.Routing.RouteLog where

import Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Void

data Query a = ChangeRoute String a

type State = { history :: Array String }

component :: forall i o m. H.Component HH.HTML Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }

initialState :: forall i. i -> State
initialState _ = { history: [] }

render :: forall act m. State -> H.ComponentHTML act () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Change the URL hash or choose an anchor link..." ]
    , HH.ul_
        [ HH.li_ [ HH.a [ HP.href "#link-a" ] [ HH.text "Link A" ] ]
        , HH.li_ [ HH.a [ HP.href "#link-b" ] [ HH.text "Link B" ] ]
        , HH.li_ [ HH.a [ HP.href "#link-c" ] [ HH.text "Link C" ] ]
        ]
    , HH.p_ [ HH.text "...to see it logged below:" ]
    , HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.history
    ]

handleQuery :: forall act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
handleQuery = case _ of
  ChangeRoute msg a -> do
    H.modify_ \st -> { history: st.history `A.snoc` msg }
    pure (Just a)
```

Here we have a router component,  it receives a Query whenever the browser route changes - in this case the query is always `ChangeRoute`. However we could also add a query which does not modify the component state, but simply returns the route information. In this way, code outside the component can both modify and retreive the information local to the component.

In this example, the `ChangeRoute` query is called from the `main` function within a continuation, or callback, which then executes when the route changes.

``` purescript
module Example.Driver.Routing.Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Example.Driver.Routing.RouteLog as RouteLog
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: CR.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CRA.produce \emitter -> do
  listener <- DOM.eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
  liftEffect $
    DOM.window
      >>= Window.toEventTarget
      >>> DOM.addEventListener HCET.hashchange listener false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer
  :: (forall a. RouteLog.Query a -> Aff (Maybe a))
  -> CR.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  void $ query $ H.tell $ RouteLog.ChangeRoute hash
  pure Nothing

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI RouteLog.component unit body

  -- Connecting the consumer to the producer initializes both, adding the event
  -- listener to the window and feeding queries back to our component as events
  -- are received.
  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
```

Here we're using the Coroutine module to create an event producer and consumer, allowing us to run the query with the `tell` function and the `query` function on the `HalogenIO` record (`io.query`).

Let's examine these functions at a closer level. First, `handleQuery`:

``` purescript
handleQuery :: forall act o m a. Query a -> H.HalogenM State act () o m (Maybe a)
```

`handleQuery` accepts some `Query a`, which refers to the component's Query type. It returns a `HalogenM` with the new `State` value (which may or may not have been modified), along with the other `HalogenM` parameters, the last parameter is `(Maybe a)`, `a` here should always be `unit`, however since we cannot prove that, we leave it as a type variable.

The `tell` function is used to build a query which does not expect a meaningful response, let's examine its type:

``` purescript
type Tell f = Unit -> f Unit

tell :: forall f. Tell f -> f Unit
```

`tell` takes the data constructor of a query type `f` and creates a query.
The query is then passed to the `query` function, which runs the query:

``` purescript
query :: forall a. query a -> m (Maybe a)
```

This function will take a well formed query (assembled by the `tell` function for example), and runs it through the component tree.

In the case of `tell`, we do not expect a meaningful response (we can see in the type that `tell` will hold a `Unit` value). However,  there are other queries which can return meaningful values. The `request` function is used for queries which return useful information:

``` purescript
type Request f a = (a -> a) -> f a

request :: forall f a. Request f a -> f a
```

We can see that request takes a query that takes a function.

Here is an example of a request-style query that might be used in a component:

``` purescript
data Query a = SomeRequest (Boolean -> a)
```

So for request-style queries, the return type is represented by the `a`. To send this request, we might do something like this:

``` purescript
getTickCount :: forall o. H.HalogenIO Query o Aff -> Aff (Maybe Int)
getTickCount app = app.query (H.request GetTickCount)
```


Returning to our simple parent and child example in the [`components` example](../examples/components/), we can also run queries against individual components by ChildSlot using the `query` function available in the `Halogen` module.

In our example we use `query` to check what the current button state is when evaluating `CheckButtonState` for the parent:

``` purescript
CheckButtonState -> do
  buttonState <- H.query _button unit $ H.request Button.IsOn
  H.modify_ (_ { buttonState = buttonState })
```

We can see that here, it takes the `SProxy` function that accesses the correct child component, the slot (in the case where the same component is mounted multiple times this might be an integer), and a well-formed query either using `tell` or `request`.

In this function, we're setting the container component state once we have successfully queried the button component.

As it happens, the `buttonState` we're storing in the container component is `Maybe Boolean`, so we didn't have to do anything before storing it here. Often we'll need to handle the `Maybe` first however. A common pattern is to use the `Foldable` instance of `Maybe`, allowing us to write handlers like:

``` purescript
CheckButtonState next -> do
  H.query ButtonSlot (H.request Button.IsOn) >>= traverse_ \isOn ->
    -- do something with `isOn :: Boolean`
    pure unit
  pure next
```

As well as being able to query children one at a time, we can send a query to all the children of a component at once, using the [`queryAll`][Halogen.Query.queryAll] function:

``` purescript
queryAll
  :: forall state action output m label slots query output' slot a _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> query a
  -> HalogenM state action slots output m (Map slot a)
```

This sends the same query to every child that fits the `Sproxy label`, regardless of the slot, and then gives us the result back as a map where the keys are slot addresses and the values are the query result for that child.

That covers it for basic parent/child setups: the only differences between standalone and parent components are the need to define a slot type and the ability to query children.

## Input values

So far, whenever `input` values have been mentioned, they've been glossed over. Now that we know how to embed a child component within a parent, we can get into it.

Input values are a means of passing values into a child component every time a parent re-renders. It is also possible to do this by querying the children whenever a parent modifies its state, but as the `input` mechanism is declarative, it's less error prone and often more convenient.

First we'll need to set up a component that expects an input:

``` purescript
module Example.Components.Inputs.Display where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

type Slot p = forall q. H.Slot q Void p

type Input = Int

type State = Int

data Action = HandleInput Int

component :: forall q o m. H.Component HH.HTML q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleInput
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.text "My input value is:"
    , HH.strong_ [ HH.text (show state) ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput n -> do
    oldN <- H.get
    when (oldN /= n) $ H.put n
```

This is a super simple component that just displays whatever value it has received as its input. As you can see, both the `Input` and `State` type synonyms we have here are `Int`. Usually the component would have some additional private state of its own.

The next part that needs providing is the `receive` provided to `mkEval`. This function determines what to do when provided with an input value. It is a `i -> Maybe (action)` mapping, so we can choose to do nothing with some input values if we prefer.

Finally, we evaluate the action raised by the `receive` function, just as we would  any other action.  Since `receive` is triggered on every render the parent component makes, we may want to have some logic either in the `receive` function or in `handleAction` which determines whether any change is necessary.

``` purescript
handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput n -> do
    oldN <- H.get
    when (oldN /= n) $ H.put n
```

We can see in this case we are using `when (oldN /=n)` to determine whether or not to update the state, to avoid unnecessary rendering.

When we use this component in a slot now we can pass through an `Int` value based on the parent component's state and see it reflected in the child.

In the [`components-inputs` example](../examples/components-inputs) that the above code was taken from, the "display component" we defined above is used like this:

``` purescript
type ChildSlots =
  (display :: Display.Slot Int
  )

...

HH.ul_
  [ HH.slot _display 1 Display.component state absurd
  , HH.slot _display 2 Display.component (state * 2) absurd
  , HH.slot _display 3 Display.component (state * 3) absurd
  , HH.slot _display 4 Display.component (state * 10) absurd
  , HH.slot _display 5 Display.component (state * state) absurd
  ]
```

This gives us a variety of displays, each with a different permutation of the parent component's state, and all of which update in tandem with changes in the parent component's state.

## Multiple types of child component

The need for multiple types of child component under a parent arises quite often, so Halogen has some tools to help with that.

By "multiple types", an example would be where we have a component for the top level of an app, and inside that view we'd want a menu bar component, a current page view component, a dialog box component, etc. It's unlikely that all these elements would share the same query algebra, so we need a way of dealing with that.

### Querying

``` purescript
handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ReadStates -> do
    a <- H.query _a unit (H.request CA.IsOn)
    b <- H.query _b unit (H.request CB.GetCount)
    c <- H.query _c unit (H.request CC.GetValue)
    H.put { a, b, c }
```

Here, we've simply imported the various query algebras, and called them on the A, B, and C components then put the results of those queries into state.

This is vastly simplified over previous versions of Halogen.

This and most of the following code snippets are based on the [`components-multitype` example](../examples/components-multitype/).

### Rendering

Rendering is much the same as before:

``` purescript
render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state = HH.div_
  [ HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component A" ]
      , HH.slot _a unit CA.component unit absurd
      ]
  , HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component B" ]
      , HH.slot _b unit CB.component unit absurd
      ]
  , HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component C" ]
      , HH.slot _c unit CC.component unit absurd
      ]
  , HH.p_
      [ HH.text "Last observed states:"]
  , HH.ul_
      [ HH.li_ [ HH.text ("Component A: " <> show state.a) ]
      , HH.li_ [ HH.text ("Component B: " <> show state.b) ]
      , HH.li_ [ HH.text ("Component C: " <> show state.c) ]
      ]
  , HH.button
      [ HE.onClick (\_ -> Just ReadStates) ]
      [ HH.text "Check states now" ]
  ]
```


You've made it to the end of the guide, as it stands... happy Halogen-ing!

[purescript-profunctor-lenses]: https://github.com/purescript-contrib/purescript-profunctor-lenses

[Data.Either.Nested]: https://pursuit.purescript.org/packages/purescript-either/4.0.0/docs/Data.Either.Nested "Data.Either.Nested"
[Data.Either.Nested.Either2]: https://pursuit.purescript.org/packages/purescript-either/4.0.0/docs/Data.Either.Nested#t:Either2 "Data.Either.Nested.Either2"
[Data.Functor.Coproduct.Coproduct]: https://pursuit.purescript.org/packages/purescript-functors/3.0.0/docs/Data.Functor.Coproduct#t:Coproduct "Data.Functor.Coproduct.Coproduct"
[Data.Functor.Coproduct.Nested]: https://pursuit.purescript.org/packages/purescript-functors/3.0.0/docs/Data.Functor.Coproduct.Nested "Data.Functor.Coproduct.Nested"
[Data.Functor.Coproduct.Nested.Coproduct2]: https://pursuit.purescript.org/packages/purescript-functors/3.0.0/docs/Data.Functor.Coproduct.Nested#t:Coproduct2 "Data.Functor.Coproduct.Nested.Coproduct2"
[Data.Lens.Prism.prism']: https://pursuit.purescript.org/packages/purescript-profunctor-lenses/3.2.0/docs/Data.Lens.Prism#v:prism' "Data.Lens.Prism.prism'"
[Data.Lens.Types.Prism']: https://pursuit.purescript.org/packages/purescript-profunctor-lenses/3.2.0/docs/Data.Lens.Types#t:Prism' "Data.Lens.Types.Prism'"
[Halogen.Component.ChildPath.ChildPath]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath#t:ChildPath "Halogen.Component.ChildPath.ChildPath"
[Halogen.Component.ChildPath.compose]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath#v:compose "Halogen.Component.ChildPath.compose"
[Halogen.Component.ChildPath.cp1]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath#v:cp1 "Halogen.Component.ChildPath.cp1"
[Halogen.Component.ChildPath.cp10]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath#v:cp10 "Halogen.Component.ChildPath.cp10"
[Halogen.Component.ChildPath.cpL]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath#v:cpL "Halogen.Component.ChildPath.cpL"
[Halogen.Component.ChildPath.cpR]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath#v:cpR "Halogen.Component.ChildPath.cpR"
[Halogen.Component.ChildPath]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component.ChildPath "Halogen.Component.ChildPath"
[Halogen.Component.parentComponent]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component#v:parentComponent "Halogen.Component.parentComponent"
[Halogen.Component.ParentDSL]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component#t:ParentDSL "Halogen.Component.ParentDSL"
[Halogen.Component.ParentHTML]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Component#t:ParentHTML "Halogen.Component.ParentHTML"
[Halogen.HTML.slot']: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.HTML#v:slot' "Halogen.HTML.slot'"
[Halogen.HTML.slot]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.HTML#v:slot "Halogen.HTML.slot"
[Halogen.Query.HalogenM]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Query#t:HalogenM "Halogen.Query.HalogenM"
[Halogen.Query.query']: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Query#v:query' "Halogen.Query.query'"
[Halogen.Query.query]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Query#v:query "Halogen.Query.query"
[Halogen.Query.queryAll']: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Query#v:queryAll' "Halogen.Query.queryAll'"
[Halogen.Query.queryAll]: https://pursuit.purescript.org/packages/purescript-halogen/docs/Halogen.Query#v:queryAll "Halogen.Query.queryAll"
[Data.Void.absurd]: https://pursuit.purescript.org/packages/purescript-prelude/4.0.0/docs/Data.Void#v:absurd "Data.Void.absurd"
