# Introducing Components

Halogen HTML is one basic building block of Halogen applications. But pure functions that produce HTML lack many essential features that a real world application needs: state that represents values over time, effects for things like network requests, and the ability to respond to DOM events (for example, when a user clicks a button).

Halogen components accept input and produce Halogen HTML, like the functions we've seen so far. Unlike functions, though, components maintain internal state, can update their state or perform effects in response to events, and can communicate with other components.

Halogen uses a component architecture. That means that Halogen uses components to let you split your UI into independent, reusable pieces and think about each piece in isolation. You can then combine components together to produce sophisticated applications.

For example, every Halogen application is made up of at least one component, which is called the "root" component. Halogen components can contain further components, and the resulting tree of components comprises your Halogen application.

In this chapter we'll learn most of the essential types and functions for writing Halogen components. For a beginner, this is the hardest chapter in the guide because many of these concepts will be brand-new. Don't worry if it feels overwhelming the first time you read it! You'll use these types and functions over and over again when you write Halogen applications, and they soon become second nature. If you're having a hard time with the chapter, try reading it again while building a simple component other than the one described here.

In this chapter we'll also see more examples of Halogen's declarative style of programming. When you write a component you're responsible for describing what UI should exist for any given internal state. Halogen, under the hood, updates the actual DOM elements to match your desired UI.

## A Tiny Example

We have already seen a simple example of a component: a counter that can be incremented or decremented.

```purs
module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = Increment | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
      , HH.text (show state)
      , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Decrement ->
      H.modify_ \state -> state - 1

    Increment ->
      H.modify_ \state -> state + 1
```

This component maintains an integer as its internal state, and updates that state in response to click events on the two buttons.

This component works, but in a real world application we wouldn't leave all the types unspecified. Let's rebuild this component from scratch with all the types it uses.

## Building a Basic Component (With Types)

A typical Halogen component accepts input, maintains an internal state, produces Halogen HTML from that state, and updates its state or performs effects in response to events. In this case we don't need to perform any effects, but we'll cover them soon.

Let's break down each part of our component, assigning types along the way.

### Input

Halogen components can accept input from a parent component or the root of the application. If you think of a component as a function, then input is the function's argument.

If your component takes input, then you should describe it with a type. For example, a component that accepts an integer as input would use this type:

```purs
type Input = Int
```

Our counter doesn't require any input, so we have two choices. First, we can just say that our input type is `Unit`, meaning that we'll just take a dummy value and throw it away:

```purs
type Input = Unit
```

Second, and more commonly, anywhere our input type shows up in our component we can simply leave it as a type variable: `forall i. ...`. It's perfectly fine to use either approach, but from here on out we'll use type variables to represent types our component isn't using.

### State

Halogen components maintain an internal state over time, which is used to drive the component's behavior and to produce HTML. Our counter component maintains the current count, an integer, so we'll use that as our state type:

```purs
type State = Int
```

Our component needs to also produce an initial state value. All Halogen components require an `initialState` function which produces the initial state from the input value:

```purs
initialState :: Input -> State
```

Our counter component doesn't use its input, so our `initialState` function won't use an input type and will instead just leave that type variable open. Our counter should start at 0 when the component runs.

```purs
initialState :: forall input. input -> State
initialState _ = 0
```

### Actions

Halogen components can update state, perform effects, and communicate with other components in response to events that arise internally. Components use an "action" type to describe what kinds of things a component can do in response to internal events.

Our counter has two internal events:

1. a click event on the "-" button to decrement the count
2. a click event on the "+" button to increment the count.

We can describe what our component should do in response to these events using a data type we'll call `Action`:

```purs
data Action = Increment | Decrement
```

This type signifies that our component is capable of incrementing and decrementing. In a moment, we'll see this type used in our HTML -- another example of Halogen's declarative nature.

Just like how our state type had to be paired with an `initialState` function that describes how to produce a `State` value, our `Action` type should be paired with a function called `handleAction` that describes what to do when one of these actions occurs.

```purs
handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
```

As with our input type, we can leave type variables open for types that we aren't using.

- The type `()` means our component has no child components. We could also leave it open as a type variable because we aren't using it -- `slots`, by convention -- but `()` is so short you'll see this type commonly used instead.
- The `output` type parameter is only used when your component communicates with a parent.
- The `m` type parameter is only relevant when your component performs effects.

Since our counter has no child components we'll use `()` to describe them, and because it doesn't communicate with a parent or perform effects we'll leave the `output` and `m` type variables open.

Here's the `handleAction` function for our counter:

```purs
handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1
```

Our `handleAction` function responds to `Decrement` by reducing our state variable by 1, and to `Increment` by increasing our state variable by 1. Halogen provides several update functions you can use in your `handleAction` function; these ones are commonly used:

- `modify` allows you to update the state, given the previous state, returning the new state
- `modify_` is the same as `modify`, but it doesn't return the new state (thus you don't have to explicitly discard the result, as you would with `modify`)
- `get` allows you to retrieve the current state
- `gets` allows you to retrieve the current state and also apply a function to it (most commonly, `_.fieldName` to retrieve a particular field from a record)

We'll talk more about `HalogenM` when we talk about performing effects. Our counter doesn't perform effects, so all we need are the state update functions.

### Rendering

Halogen components produce HTML from their state using a function called `render`. The render function runs every time the state changes. This is what makes Halogen declarative: for any given state, you describe the UI that it corresponds to. Halogen handles the workload of ensuring that state changes always result in the UI you described.

Render functions in Halogen are pure, which means that you can't do things like get the current time, make network requests, or anything like that during rendering. All you can do is produce HTML for your state value.

When we look at the type of our render function we can see the `ComponentHTML` type we touched on last chapter. This type is a more specialized version of the `HTML` type, meant specifically for HTML produced in components. Once again, we'll use `()` and leave `m` open because they are only relevant when using child components, which we'll cover in a later chapter.

```purs
render :: forall m. State -> H.ComponentHTML Action () m
```

Now that we're working with our render function, we're back to the Halogen HTML that should be familiar from the last chapter! You can write regular HTML in `ComponentHTML` just like we did last chapter:

```purs
import Halogen.HTML.Events

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
    , HH.text (show state)
    , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
    ]
```

#### Handling Events

We can now see how to handle events in Halogen. First, you write the event handler in the properties array along with any other properties, attributes, and refs you might need. Then, you associate the event handler with an `Action` that your component knows how to handle. Finally, when the event occurs, your `handleAction` function is called to handle the event.

You might be curious about why we provided an anonymous function to `onClick`. To see why, we can look at the actual type of `onClick`:

```purs
onClick
  :: forall row action
   . (MouseEvent -> Maybe action)
  -> IProp (onClick :: MouseEvent | row) action

-- Specialized to our component
onClick
  :: forall row
   . (MouseEvent -> Maybe Action)
  -> IProp (onClick :: MouseEvent | row) Action
```

In Halogen, event handlers take as their first argument a callback. This callback receives the DOM event that occurred (in the case of a click event, that's a `MouseEvent`), which contains some metadata you may want to use, and is then responsible for returning an action that Halogen should run in response to the event (or `Nothing`, if no action should be performed). In our case, we won't inspect the event itself, so we throw the argument away and return the action we want to run (`Increment` or `Decrement`).

The `onClick` function then returns a value of type `IProp`. You should remember `IProp` from the previous chapter. As a refresher, Halogen HTML elements specify a list of what properties and events they support. Properties and events in turn specify their type. Halogen is then able to ensure that you never use a property or event on an element that doesn't support it. In this case buttons do support `onClick` events, so we're good to go!

### Bringing It All Together

Let's bring each of our types and functions back together to produce our counter component -- this time with types specified. Let's revisit the types and functions that we wrote:

```purs
-- This can be specified if your component takes input, or you can leave
-- the type variable open if your component doesn't.
type Input = Unit

type State = Int

initialState :: forall input. input -> State
initialState = ...

data Action = Increment | Decrement

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = ...

render :: forall m. State -> H.ComponentHTML Action () m
render = ...
```

These types and functions are the core building blocks of a typical Halogen component. But they aren't sufficient on their own like this -- we need to bring them all together in one place.

We'll do that using the `H.mkComponent` function. This function takes a `ComponentSpec`, which is a record containing an `initialState`, `render`, and `eval` function, and produces a `Component` from it:

```purs
component =
  H.mkComponent
    { -- First, we provide our function that describes how to produce the first state
      initialState
      -- Then, we provide our function that describes how to produce HTML from the state
    , render
      -- Finally, we provide our function that describes how to handle actions that
      -- occur while the component is running, which updates the state.
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
```

We'll talk more about the `eval` function in future chapters. For the time being you can think of the `eval` function as defining how the component responds to events; for now, the only kind of events we care about are actions, and so the only function we'll use is `handleAction`.

Our component is now complete, but we're missing one last type definition: our component type.

### The `H.Component` Type

The `mkComponent` function produces a component from a `ComponentSpec`, which is a record of the functions that Halogen needs to run a component. We'll get into more detail about this type in a subsequent chapter.

```purs
mkComponent :: H.ComponentSpec ... -> H.Component query input output m
```

The resulting component has the type `H.Component`, which itself takes five type parameters that describe the public interface of the component. Our component doesn't communicate with parent components or child components, so it doesn't use any of these type variables. Still, we'll briefly step through them now so you know what's coming in subsequent chapters.

1. The first parameter `query` represents a way that parent components can communicate with this component. We will talk about it more when we talk about parent and child components.
1. The second parameter `input` represents the input our component accepts. In our case, the component doesn't accept any input, so we'll leave this variable open.
1. The third parameter `output` represents a way that this component can communicate with its parent component. We'll talk about it more when we talk about parent and child components.
1. The final parameter, `m`, represents the monad that can be used to run effects in the component. Our component doesn't run any effects, so we'll leave this variable open.

Our counter component can therefore be specified by leaving all of the `H.Component` type variables open except for the first one, `HH.HTML`.

## The Final Product

That was a lot to take in! We've finally got our counter component fully specified with types. If you can comfortably build components like this one, you're most of the way to a thorough understanding of building Halogen components in general. The rest of this guide will build on top of your understanding of state, actions, and rendering HTML.

We've added a `main` function that runs our Halogen application so that you can try this example out by pasting it into [Try PureScript](https://try.purescript.org). We'll cover how to run Halogen applications in a later chapter -- for now you can ignore the `main` function and focus on the component we've defined.

```purs
module Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State = Int

data Action = Increment | Decrement

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "-" ]
    , HH.text (show state)
    , HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "+" ]
    ]

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1
```
