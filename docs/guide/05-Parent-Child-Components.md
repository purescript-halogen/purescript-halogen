# Parent and Child Components

Halogen is an unopinionated UI library: it allows you to create declarative user interfaces without enforcing a particular architecture.

Our applications so far have consisted of a single Halogen component. You can build large applications as a single component and break the state and the `handleAction` and `render` functions into separate modules as the app grows. This lets you use the Elm architecture in Halogen.

However, Halogen supports architectures with arbitrarily deep trees of components. That means any component you write is allowed to contain more components, each with their own state and behaviors. Most Halogen applications use a component architecture in this way, including the [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) app.

When you move from a single component to many components you begin to need mechanisms so that components can communicate with one another. Halogen gives us three ways for a parent and child component to communicate:

1. A parent component can send _queries_ to a child component, which either tell the child component to do something or request some information from it.
2. A parent component gives a child component the _input_ it needs, which is re-sent every time the parent component renders.
3. A child component can emit _output messages_ to the parent component, notifying it when an important event has occurred.

These type parameters are represented in the `Component` type, and some are also found in the `ComponentHTML` and `HalogenM` types. For example, a component that supports queries, input, and output messages will have this `Component` type:

```purs
component :: forall m. H.Component Query Input Output m
```

You can think of the ways a component can communicate with other components as its _public interface_, and the public interface shows up in the `Component` type.

In this chapter we'll learn about:

1. How to render components in your Halogen HTML
2. The three ways that components communicate: queries, input, and output messages
3. Component slots, the `slot` function, and the `Slot` type, which make this communication type-safe

We'll start by rendering a simple child component that has no queries or output messages. Then, we'll build up components that use these ways to communicate, ending with a final example that shows off a parent and child component using all of these mechanisms at once.

Try loading the example into Try PureScript to explore each of the communication mechanisms discussed in this chapter!

## Rendering Components

We began this guide by writing functions that returned Halogen HTML elements. These functions could be used by other functions to build even larger trees of HTML elements.

When we started using components we began writing `render` functions. Conceptually, components produce Halogen HTML as their result via this function, though they can also maintain internal state and perform effects, among other things.

In fact, while we've only been using HTML elements when writing our `render` functions so far, we can also use _components_ as if they were functions that produce HTML. The analogy is imperfect, but it can be a helpful mental model for understanding how to treat components when you are writing your `render` function.

When one component renders another, it's called the "parent" component and the component it renders is called the "child" component.

Let's see how we can render a component inside our `render` function, instead of only HTML elements as we've seen so far. We'll start by writing a component that uses a helper function to render a button. Then, we'll turn that helper function into its own component, and we'll adjust the parent component to render this new child component.

First, we'll write a component that uses a helper function to render some HTML:

```purs
module Main where

import Prelude

import Halogen as H
import Halogen.HTML as HH

parent :: forall query input output m. H.Component query input output m
parent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: forall state action. state -> H.ComponentHTML action () m
  render _ = HH.div_ [ button { label: "Click Me" } ]

button :: forall w i. { label :: String } -> HH.HTML w i
button { label } = HH.button [ ] [ HH.text label ]
```

This should look familiar. We have a simple component that renders a `div`, and a helper function, `button`, which renders a button given a label as input. As a note, our `parent` component leaves type variables open for our state and actions because it doesn't have an internal state and it doesn't have any actions.

Now, let's turn our `button` function into a component for demonstration purposes (in a real world app it would be too small for that):

```purs
type Input = { label :: String }

type State = { label :: String }

button :: forall query output m. H.Component query Input output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: Input -> State
  initialState input = input

  render :: forall action. State -> H.ComponentHTML action () m
  render { label } = HH.button [ ] [ HH.text label ]
```

We took a few steps to convert our button HTML function into a button component:

1. We converted the argument to our helper function into the `Input` type for the component. The parent component is responsible for providing this input to our component. We'll learn more about input in the next section.
2. We moved our HTML into the component's `render` function. The `render` function only has access to our component's `State` type, so in our `initialState` function we copied our input value into our state so we could render it. Copying input into state is a common pattern in Halogen. Also notice that our `render` function leaves the action type unspecified (because we don't have any actions) and indicates we have no child components using `()`.
3. We used `defaultEval`, unmodified, as our `EvalSpec` because this component doesn't need to respond to events arising internally -- it has no actions and uses no lifecycle events, for example.

Our parent component is now broken, though! If you've been following along, you'll now see an error:

```purs
[1/1 TypesDoNotUnify]

  16    render _ = HH.div_ [ button { label: "Click Me" } ]
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Could not match type

    Component HTML t2 { label :: String }

  with type

    Function
```

Components can't just be rendered by giving the component its input as a function argument. Even though components produce ordinary Halogen HTML they can also communicate with the parent component; for this reason, components need extra information before they can be rendered like an ordinary element.

Conceptually, components occupy a "slot" in your tree of HTML. This slot is a place where the component can produce Halogen HTML until it is removed from the DOM. A component in a slot can be thought of as a dynamic, stateful HTML element. You can freely intermix these dynamic elements with ordinary Halogen HTML elements, but the dynamic elements need more information.

That extra information comes from the `slot` function and the slot type used in `ComponentHTML`, which we've so far been leaving as the empty row, `()`. We'll talk a lot more about rendering components in slots in a moment, but for now let's get things compiling.

We can fix our `render` function by rendering our component in a slot via the `slot` function. We'll also update the slot type in our `ComponentHTML` to include the component our Halogen HTML now must support. This diff demonstrates the differences between rendering an HTML element and rendering a component:

```diff
+ import Type.Proxy (Proxy(..))
+
+ type Slots = ( button :: forall query. H.Slot query Void Int )
+
+ _button = Proxy :: Proxy "button"

  parent :: forall query input output m. H.Component query input output m
  parent =
    H.mkComponent
      { initialState: identity
      , render
      , eval: H.mkEval H.defaultEval
      }
    where
-   render :: forall state action. state -> H.ComponentHTML action () m
+   render :: forall state action. state -> H.ComponentHTML action Slots m
    render _ =
-     HH.div_ [ button { label: "Click Me" } ]
+     HH.div_ [ HH.slot_ _button 0 button { label: "Click Me" } ]
```

Our parent component is now rendering a child component -- our button component. Rendering a component introduced two big changes:

1. We used the `slot_` function to render the component, which takes several arguments we haven't explored yet. Two of those arguments are the `button` component itself and the label it needs as input.
2. We added a new type called `Slots`, which is a row containing a label for our button component with a value of type `H.Slot`, and we used this new type in our `ComponentHTML` instead of the previous empty row `()` we've seen so far.

The `slot` and `slot_` functions and the `Slot` type let you render a stateful, effectful child component in your Halogen HTML as if it were any other HTML element. But why are there so many arguments and types involved in doing this? Why can't we just call `button` with its input?

The answer is that Halogen provides two ways for a parent and child component to communicate with one another, and we need to ensure that this communication is type-safe. The `slot` function allows us to:

1. Decide how to identify a particular component by a label (the type-level string "button", which we represent at the term level with the proxy `Proxy :: Proxy "button"`) and a unique identifier (the integer `0`, in this case) so that we can send it _queries_. This is an imperative form of communication from the parent to the child.
2. Render the component (`button`) and give it its _input_ (`{ label: "Click Me" }`), which will be re-sent every time the parent component renders in case the input changes over time. This is a declarative form of communication from the parent to the child.
3. Decide how to handle _output messages_ from the child component. The `slot` function lets you provide a handler for child outputs, while the `slot_` function can be used when a child component doesn't have any outputs or you want to ignore them. This is communication from the child to the parent.

The `slot` and `slot_` functions and the `H.Slot` type let us manage these three communication mechanisms in a type-safe way. In the rest of this chapter we'll focus on how parent and child components communicate with one another, and along the way we'll explore slots and slot types.

## Communicating Among Components

When you move from using one component to using many components you'll soon need some way for them to communicate with one another. In Halogen there are three ways that a parent and child component can communicate directly:

1. The parent component can provide input to the child component. Each time the parent component renders it will send the input again, and then it's up to the child component to decide what to do with the new input.
2. The child component can emit output messages to the parent, similar to how we've been using subscriptions so far. The child component can notify the parent component when an important event has happened, like a modal closing or a form being submitted, and then the parent can decide what to do.
3. The parent component can query the child component, either by telling it to do something or by requesting some information from it. The parent component can decide when it needs the child component to do something or give it some information, and then it's up to the child component to handle the query.

These three mechanisms give you several ways to communicate between components. Let's briefly explore these three mechanisms, and then we'll see how the `slot` function and the slot type you define for your component help you use them in a type-safe way.

### Input

Parent components can provide input to child components, which is sent on every render. We've seen this several times already -- the `input` type is used to produce the child component's initial state. In the example which introduced this chapter our button component received its label from the parent component.

So far we've only used input to produce our initial state. But input doesn't stop once the initial state has been created. The input is sent again on every render, and the child component can handle the new input via the `receive` function in its eval spec.

```purs
receive :: input -> Maybe action
```

The `receive` function in the eval spec should remind you of `initialize` and `finalize`, which let you choose an action to evaluate when the component is created and destroyed. In the same way, the `receive` function lets you choose an action to evaluate when the parent component sends new input.

By default Halogen's `defaultSpec` doesn't provide an action to be evaluated when new input is received. If your child component doesn't need to do anything after it receives its initial value then you can leave this as-is. For example, once our button received its label and copied it into state there was no need to continue listening to the input in case it changed over time.

The ability to receive new input every time the parent renders is a powerful feature. It means parent components can declaratively provide values to child components. There are other ways for a parent component to communicate with a child component, but the declarative nature of input makes it the best choice in most circumstances.

Let's make this concrete by revisiting our example from the introduction. In this version our button is unchanged -- it receives its label as input and uses it to set its initial state -- but our parent component has changed. Our parent component now starts a timer when it initializes, increments a count every second, and uses the count in state as the label for the button.

In short, our button's input will be re-sent every second. Try pasting this into [Try PureScript](https://try.purescript.org) to see what happens -- does our button's label update every second?

```purs
module Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI parent unit body

type Slots = ( button :: forall q. H.Slot q Void Unit )

_button = Proxy :: Proxy "button"

type ParentState = { count :: Int }

data ParentAction = Initialize | Increment

parent :: forall query input output m. MonadAff m => H.Component query input output m
parent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  initialState :: input -> ParentState
  initialState _ = { count: 0 }

  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render { count } =
    HH.div_ [ HH.slot_ _button unit button { label: show count } ]

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      { emitter, listener } <- H.liftEffect HS.create
      void $ H.subscribe emitter
      void
        $ H.liftAff
        $ Aff.forkAff
        $ forever do
            Aff.delay $ Milliseconds 1000.0
            H.liftEffect $ HS.notify listener Increment
    Increment -> H.modify_ \st -> st { count = st.count + 1 }

-- Now we turn to our child component, the button.

type ButtonInput = { label :: String }

type ButtonState = { label :: String }

button :: forall query output m. H.Component query ButtonInput output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label }

  render :: forall action. ButtonState -> H.ComponentHTML action () m
  render { label } = HH.button_ [ HH.text label ]
```

If you load this into Try PureScript you'll see that our button...never changes! Even though the parent component is sending it new input every second (every time the parent re-renders) our child component is never receiving it. It's not enough to accept input; we also need to explicitly decide what to do each time it is received.

Try replacing the button code with this revised code to see the difference:

```purs
data ButtonAction = Receive ButtonInput

type ButtonInput = { label :: String }

type ButtonState = { label :: String }

button :: forall query output m. H.Component query ButtonInput output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label }

  render :: ButtonState -> H.ComponentHTML ButtonAction () m
  render { label } = HH.button_ [ HH.text label ]

  handleAction :: ButtonAction -> H.HalogenM ButtonState ButtonAction () output m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input ->
      H.modify_ _ { label = input.label }
```

We made several changes in the new version to ensure we stayed up-to-date with input from the parent component:

1. We added a new action, `Receive`, a constructor that accepts the `Input` type as its argument. We then handled this action in our `handleAction` function by updating our state when new input is received.
2. We added a new field to our eval spec, `receive`, which holds a function that will be called every time new input is received. Our function returns our `Receive` action so it can be evaluated.

This change is sufficient to subscribe our child component to new input from the parent component. You should now see that our button's label updates every second. As an exercise, you can replace our `receive` function with `const Nothing` to see the how the input is ignored once again.

### Output Messages

Sometimes an event happens in a child component that it shouldn't handle itself.

For example, let's say we're writing a modal component, and we need to evaluate some code when a user clicks to close the modal. To keep this modal flexible we'd like for the parent component to decide what should happen when the modal is closed.

In Halogen we'd handle this situation by designing the modal (the child component) to raise an **output message** to the parent component. The parent component can then handle the message like any other action in its `handleAction` function. Conceptually, it's as though the child component is a subscription that the parent component automatically subscribes to.

Concretely, our modal could raise a `Closed` output to the parent component. The parent could then change its state to indicate the modal should no longer display, and on the next render the modal is removed from the DOM.

As a tiny example, let's consider how we'd design a button that lets the parent component decide what to do when it is clicked:

```purs
module Button where

-- This component can notify parent components of one event, `Clicked`
data Output = Clicked

-- This component can handle one internal event, `Click`
data Action = Click

-- Our output type shows up in our `Component` type
button :: forall query input m. H.Component query input Output m
button =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render _ =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text "Click me" ]

  -- Our output type also shows up in our `HalogenM` type, because this is
  -- where we can emit these output messages.
  handleAction :: forall state. Action -> H.HalogenM state Action () Output m Unit
  handleAction = case _ of
    -- When the button is clicked we notify the parent component that the
    -- `Clicked` event has happened by emitting it with `H.raise`.
    Click ->
      H.raise Clicked
```

We took a few steps to implement this output message.

1. We added an `Output` type which describes what output messages our component can emit. We used the type in our `Component` type because it's part of the component's public interface and our `HalogenM` type because this is where we can actually emit the output message.
1. We added an `Action` type with a `Click` constructor to handle the click event in our Halogen HTML
1. We handled the `Click` action in our `handleAction` by _raising_ an output message to the parent component. You can emit output messages with the `H.raise` function.

We now know how a component can emit output messages. Now, let's see how to handle output messages from a child component. There are three things to keep in mind:

1. When you render a child component you will need to add it to your slots type, which is then used in your `ComponentHTML` and `HalogenM` types. The type you add will include the child component's output message type, which allows the compiler to verify your handler.
2. When you render a child component with the `slot` function you can provide an action that should be evaluated when new output arises. This is similar to how lifecycle functions like `initialize` accept an action to evaluate when the component initializes.
3. Then, you'll need to add a case to your `handleAction` for the action you added to handle the child component's output.

Let's start writing our parent component by writing a slot type:

```purs
module Parent where

import Button as Button

type Slots = ( button :: forall query. H.Slot query Button.Output Int )

-- We can refer to the `button` label using a symbol proxy, which is a
-- way to refer to a type-level string like `button` at the value level.
-- We define this for convenience, so we can use _button to refer to its
-- label in the slot type rather than write `Proxy` over and over.
_button = Proxy :: Proxy "button"
```

Our slot type is a row, where each label designates a particular _type_ of child component we support, in each case using the type `H.Slot`:

```purs
H.Slot query output id
```

This type records the queries that can be sent to this type of component, the output messages that we can handle from the component, and a type we can use to uniquely identify an individual component.

Consider, for example, that we could render 10 of these button components -- how would you know which one to send a query to? That's where the slot id comes into play. We'll learn more about that when we discuss queries.

Our parent component's row type makes it clear that we can support one type of child component, which we can reference with the symbol `button` and an identifier of type `Int`. We can't send queries to this component because the type variable was left open. But it can send us outputs of type `Button.Output`.

Next, we need to provide an action for handling these outputs:

```purs
data Action = HandleButton Button.Output
```

When this action occurs in our component, we can unwrap it to get the `Button.Output` value and use that to decide what code to evaluate. Now that we have our slot and action types handled, let's write our parent component:

```purs
parent :: forall query input output m. H.Component query input output m
parent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: forall state. state -> H.ComponentHTML Action Slots m
  render _ =
    HH.div_
      [ HH.slot _button 0 button unit HandleButton ]

  handleAction :: forall state. Action -> H.HalogenM state Action Slots output m Unit
  handleAction = case _ of
    HandleButton output ->
      case output of
        Button.Clicked -> do
          ...
```

You'll notice that our `Slots` type has now been used in both the `ComponentHTML` type and the `HalogenM` type. Also, this component is now notified any time the `Button.Clicked` event happens in the child component, which lets the parent component evaluate whatever code it wants in response.

And that's it! You now know how to raise output messages from a child component to a parent component and how to then handle those messages in the parent component. This is the primary way a child component can communicate with a parent component. Now let's see how a parent component can send information to a child component.

### Queries

Queries represent commands or requests that a parent component can send to a child component. They're similar to actions and are handled with a `handleQuery` function similar to the `handleAction` function. But they arise from _outside_ the component, instead of internally within the component as actions are, which means they are part of the public interface of a component.

Queries are most useful when a parent component needs to control when an event occurs instead of a child component. For example:

- A parent component can _tell_ a form to submit, rather than wait for a user to click a submit button.
- A parent component can _request_ the current selections from an autocomplete, rather than wait for an output message from the child component when a selection is made.

Queries are a way for parent components to imperatively control a child component. As introduced in our two examples, there are two common styles of query: a tell-style query for when a parent component commands a child component to do something, and a request-style query for when a parent component wants information from a child component.

The parent component can send a query, but the child component defines the query and also handles the query. That makes queries similar conceptually to actions: just like how you define an `Action` type and handle actions for your component with `handleAction`, you define a `Query` type and a `handleQuery` function for queries.

Here's a brief example of a query type that includes a tell-style and request-style query:

```purs
data Query a
  = Tell a
  | Request (Boolean -> a)
```

We can interpret this query as meaning "A parent component can tell this component to do something with the `tell` function and it can request a `Boolean` from this component with the `request` function." When you implement a query type, remember that the `a` type parameter should be present in every constructor. It should be the final argument for tell-style queries and be the result of a function type for request-style queries.

Queries are handled with a `handleQuery` function in your eval spec, just like how actions are handled with a `handleAction` function. Let's write a `handleQuery` function for our custom data type, assuming some state, action, and output types have already been defined:

```purs
handleQuery :: forall a m. Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Tell a ->
    -- ... do something, then return the `a` we received
    pure (Just a)

  Request reply ->
    -- ... do something, then provide the requested `Boolean` to the `reply`
    -- function to produce the `a` we need to return
    pure (Just (reply true))
```

The `handleQuery` function takes a query of type `Query a` and produces some `HalogenM` code that returns `Maybe a`. This is why each constructor of our query type needs to contain an `a`: we need to return it in `handleQuery`.

When we receive a tell-style query we can just wrap the `a` we received in `Just` to return it, as we did to handle the `Tell a` case in `handleQuery`.

When we receive a request-style query, though, we have to do a little more work. Instead of receiving an `a` value we can return, we receive a function that will give us an `a` that we can then return. For example, in our `Request (Boolean -> a)` case, we receive a function that will give us an `a` when we apply it to a `Boolean`. By convention this function is called `reply` when you pattern match on a request-style query. In `handleQuery` we gave this function `true` to get an `a`, then wrapped the `a` in `Just` to return it.

Request-style queries may look strange at first. But the style allows our query type to return _many_ types of values instead of only one type of value. Here are a few different request types that return different things:

```purs
data Requests a
  = GetInt (Int -> a)
  | GetRecord ({ a :: Int, b :: String } -> a)
  | GetString (String -> a)
  | ...
```

A parent component can use `GetInt` to retrieve an `Int` from our component, `GetString` to retrieve a `String` from our component, and so on. You can consider `a` the type returned by the query type, and request-style queries a way to let `a` be many different possible types. In a moment we'll see how to do this from a parent component.

Let's see another tiny example that demonstrates how to define and handle queries in a component.

```purs
-- This component can be told to increment or can answer requests for
-- the current count
data Query a
  = Increment a
  | GetCount (Int -> a)

type State = { count :: Int }

-- Our query type shows up in our `Component` type
counter :: forall input output m. H.Component Query input output m
counter =
  H.mkComponent
    { initialState: \_ -> { count: 0 }
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where
  render { count } =
    HH.div_
      [ HH.text $ show count ]

  -- We write a function to handle queries when they arise.
  handleQuery :: forall action a. Query a -> H.HalogenM State action () output m (Maybe a)
  handleQuery = case _ of
    -- When we receive the `Increment` query we'll increment our state.
    Increment a -> do
      H.modify_ \state -> state { count = state.count + 1 }
      pure (Just a)

    -- When we receive the `GetCount` query we'll respond with the state.
    GetCount reply -> do
      { count } <- H.get
      pure (Just (reply count))
```

In this example we've defined a counter that lets the parent _tell_ it to increment or _request_ its current count. To do this, we:

1. Implemented a query type that includes a tell-style query, `Increment a`, and a request-style query, `GetCount (Int -> a)`. We added this query type to our component's public interface, `Component`.
2. Implemented a query handler, `handleQuery`, that runs code when these queries arise. We'll add this to our `eval`.

We now know how to define queries and evaluate them in a child component. Now, let's see how to _send_ a query to a child component from a parent component. As usual, we can start by defining our parent component's slot type:

```purs
module Parent where

type Slots = ( counter :: H.Slot Counter.Query Void Int )

_counter = Proxy :: Proxy "counter"
```

Our slot type records the counter component with its query type and leaves its output message type as `Void` to indicate there are none.

When our parent component initializes, we'll fetch the count from the child component, then increment it, and then get the count again so we can see that it has increased. To do that, we'll need an action to run on initialize:

```purs
data Action = Initialize
```

Now, we can move on to our component definition.

```purs
parent :: forall query input output m. H.Component query input output m
parent =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }
  where
  render :: forall state. state -> H.ComponentHTML Action Slots m
  render _ =
    HH.div_
      [ HH.slot_ _counter unit counter unit ]

  handleAction :: forall state. Action -> H.HalogenM state Action Slots output m Unit
  handleAction = case _ of
    Initialize ->
      -- startCount :: Maybe Int
      startCount <- H.request _counter unit Counter.GetCount
      -- _ :: Maybe Unit
      H.tell _counter unit Counter.Increment
      -- endCount :: Maybe Int
      endCount <- H.request _counter unit Counter.GetCount

      when (startCount /= endCount) do
        -- ... do something
```

There are several things to notice here.

1. We used the proxy for the counter's label in the slot type, `_counter`, along with its identifier, `unit`, both to render the component with the `slot` function and also to send queries to the component with the `tell` and `request` functions. The label and identifier are always used to work with a particular child component.
2. We used the `H.tell` function to send the tell-style query `Increment`, and we used the `H.request` function to send the request-style query `GetCount`. The `GetCount` query had a reply function of type `(Int -> a)`, so you'll notice that when we used it we received a `Maybe Int` in return.

The `tell` and `request` functions take a label, a slot identifier, and a query to send. The `tell` function doesn't return anything, but the `request` function returns a response from the child wrapped in `Maybe`, where `Nothing` signifies that the query failed (either the child component returned `Nothing`, or no component exists at the label and slot identifier you provided). There are also `tellAll` and `requestAll` functions that send the same query to _all_ components at a given label.

Many people find queries to be the most confusing part of the Halogen library. Luckily, queries aren't used nearly so much as the other Halogen features we've learned about in this guide, and if you get stuck you can always return to this section of the guide as a reference.

## Component Slots

We've learned a lot about how components communicate with one another. Before we move on to our final example let's recap what we've learned about slots along the way.

A component needs to know what types of child component its supports so that it's able to communicate with them. It needs to know what queries it can send to them and what output messages it can receive from them. It also needs to know how to identify which particular component to send a query to.

The `H.Slot` type captures the queries, outputs, and unique identifier for a particular type of child component the parent component can support. You can combine many slots together into a _row_ of slots, where each label is used for a particular type of component. Here's how you could read the type definitions for a few different slots:

```purs
type Slots = ()
```

This means the component supports no child components.

```purs
type Slots = ( button :: forall query. H.Slot query Void Unit )
```

This means the component supports one type of child component, identified by the symbol `button`. You can't send queries to it (because `q` is an open type variable) and it doesn't emit any output messages (usually represented with `Void` so you can use `absurd` as the handler). You can have at most one of this component because only one value, `unit`, inhabits the `Unit` type.

```purs
type Slots = ( button :: forall query. H.Slot query Button.Output Int )
```

This type is quite similar to previous one. The difference is that the child component can raise output messages of type `Button.Output`, and you can have as many of this component as there are integers.

```purs
type Slots =
  ( button :: H.Slot Button.Query Void Int
  , modal :: H.Slot Modal.Query Modal.Output Unit
  )
```

This slot type means the component supports two types of child component, identified by the labels `button` and `modal`. You can send queries of type `Button.Query` to the button component, and you won't receive any output messages from it. You can send queries of type `Modal.Query` to and receive messages of type `Modal.Output` from the modal component. You can have as many of the button component as there are integers, but at most one modal component.

A common pattern in Halogen apps is for a component to export its own slot type, because it already knows its query and messages types, without exporting the type that identifies this particular component because that's the parent's responsibility.

For example, if the button and modal component modules exported their own slot types, like this:

```purs
module Button where

type Slot id = H.Slot Query Void id

module Modal where

type Slot id = H.Slot Query Output id
```

Then our last slot type example would become this simpler type:

```purs
type Slots =
  ( button :: Button.Slot Int
  , modal :: Modal.Slot Unit
  )
```

This has the advantage of being more concise and easier to keep up-to-date over time, as if there are changes to the slot type they can happen in the source module instead of everywhere the slot type is used.

## Full Example

To wrap up, we've written an example of a parent and child component using all the communication mechanisms we've discussed in this chapter. The example is annotated with how we'd interpret the most important lines of code -- what we'd glean by skimming through these component definitions in our own codebases.

As usual, we suggest pasting this code into [Try PureScript](https://try.purescript.org) so you can explore it interactively.

```purs
module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI parent unit body

-- The parent component supports one type of child component, which uses the
-- `ButtonSlot` slot type. You can have as many of this type of child component
-- as there are integers.
type Slots = ( button :: ButtonSlot Int )

-- The parent component can only evaluate one action: handling output messages
-- from the button component, of type `ButtonOutput`.
data ParentAction = HandleButton ButtonOutput

-- The parent component maintains in local state the number of times all its
-- child component buttons have been clicked.
type ParentState = { clicked :: Int }

-- The parent component uses no query, input, or output types of its own. It can
-- use any monad so long as that monad can run `Effect` functions.
parent :: forall query input output m. MonadEffect m => H.Component query input output m
parent =
  H.mkComponent
    { initialState
    , render
      -- The only internal event this component can handle are actions as
      -- defined in the `ParentAction` type.
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: input -> ParentState
  initialState _ = { clicked: 0 }

  -- We render three buttons, handling their output messages with the `HandleButton`
  -- action. When our state changes this render function will run again, each time
  -- sending new input (which contains a new label for the child button component
  -- to use.)
  render :: ParentState -> H.ComponentHTML ParentAction Slots m
  render { clicked } = do
    let clicks = show clicked
    HH.div_
      [ -- We render our first button with the slot id 0
        HH.slot _button 0 button { label: clicks <> " Enabled" } HandleButton
        -- We render our second button with the slot id 1
      , HH.slot _button 1 button { label: clicks <> " Power" } HandleButton
        -- We render our third button with the slot id 2
      , HH.slot _button 2 button { label: clicks <> " Switch" } HandleButton
      ]

  handleAction :: ParentAction -> H.HalogenM ParentState ParentAction Slots output m Unit
  handleAction = case _ of
    -- We handle one action, `HandleButton`, which itself handles the output messages
    -- of our button component.
    HandleButton output -> case output of
      -- There is only one output message, `Clicked`.
      Clicked -> do
        -- When the `Clicked` message arises we will increment our clicked count
        -- in state, then send a query to the first button to tell it to be `true`,
        -- then send a query to all the child components requesting their current
        -- enabled state, which we log to the console.
        H.modify_ \state -> state { clicked = state.clicked + 1 }
        H.tell _button 0 (SetEnabled true)
        on <- H.requestAll _button GetEnabled
        logShow on

-- We now move on to the child component, a component called `button`.

-- This component can accept queries of type `ButtonQuery` and send output
-- messages of type `ButtonOutput`. This slot type is exported so that other
-- components can use it when constructing their row of slots.
type ButtonSlot = H.Slot ButtonQuery ButtonOutput

-- We think our button will have the label "button" in the row where it's used,
-- so we're exporting a symbol proxy for convenience.
_button = Proxy :: Proxy "button"

-- This component accepts two queries. The first is a request-style query that
-- lets a parent component request a `Boolean` value from us. The second is a
-- tell-style query that lets a parent component send a `Boolean` value to us.
data ButtonQuery a
  = GetEnabled (Boolean -> a)
  | SetEnabled Boolean a

-- This component can notify parent components of one event, `Clicked`
data ButtonOutput
  = Clicked

-- This component can handle two internal actions. It can evaluate a `Click`
-- action and it can receive new input when its parent re-renders.
data ButtonAction
  = Click
  | Receive ButtonInput

-- This component accepts a label as input
type ButtonInput = { label :: String }

-- This component stores a label and an enabled flag in state
type ButtonState = { label :: String, enabled :: Boolean }

-- This component supports queries of type `ButtonQuery`, requires input of
-- type `ButtonInput`, and can send outputs of type `ButtonOutput`. It doesn't
-- perform any effects, which we can tell because the `m` type parameter has
-- no constraints.
button :: forall m. H.Component ButtonQuery ButtonInput ButtonOutput m
button =
  H.mkComponent
    { initialState
    , render
      -- This component can handle internal actions, handle queries sent by a
      -- parent component, and update when it receives new input.
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: ButtonInput -> ButtonState
  initialState { label } = { label, enabled: false }

  -- This component has no child components. When the rendered button is clicked
  -- we will evaluate the `Click` action.
  render :: ButtonState -> H.ComponentHTML ButtonAction () m
  render { label, enabled } =
    HH.button
      [ HE.onClick \_ -> Click ]
      [ HH.text $ label <> " (" <> (if enabled then "on" else "off") <> ")" ]

  handleAction
    :: ButtonAction
    -> H.HalogenM ButtonState ButtonAction () ButtonOutput m Unit
  handleAction = case _ of
    -- When we receive new input we update our `label` field in state.
    Receive input ->
      H.modify_ _ { label = input.label }

    -- When the button is clicked we update our `enabled` field in state, and
    -- we notify our parent component that the `Clicked` event happened.
    Click -> do
      H.modify_ \state -> state { enabled = not state.enabled }
      H.raise Clicked

  handleQuery
    :: forall a
     . ButtonQuery a
    -> H.HalogenM ButtonState ButtonAction () ButtonOutput m (Maybe a)
  handleQuery = case _ of
    -- When we receive a the tell-style `SetEnabled` query with a boolean, we
    -- set that value in state.
    SetEnabled value next -> do
      H.modify_ _ { enabled = value }
      pure (Just next)

    -- When we receive a the request-style `GetEnabled` query, which requires
    -- a boolean result, we get a boolean from our state and reply with it.
    GetEnabled reply -> do
      enabled <- H.gets _.enabled
      pure (Just (reply enabled))
```

In the next chapter we'll learn more about running Halogen applications.
