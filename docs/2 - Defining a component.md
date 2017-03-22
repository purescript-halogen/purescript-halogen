# Defining a component

Here's a somewhat contrived example of a Halogen component:

``` purescript
import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

myButton :: forall m. H.Component HH.HTML Query Input Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)
```

A runnable version of this is available in the [`basic` example](../examples/basic/).

This is a component for a button that displays its current state as the label, and is toggled when clicked by a user. The rest of this chapter will break down the elements involved here.

## State

For this button component, we just need a simple state type: is the button on or off?

``` purescript
type State = Boolean
```

Often a component's state type will be a synonym for a record, as the state will need to include more elements than we have here.

## Query algebra

The type that specifies the queries for a component is known as the _query algebra_. Query algebras are of kind `Type -> Type` (meaning they always have a type variable). This type variable allows us to specify the return type of a query when we construct it. Constructors in a query algebra define operations on the component that we call _actions_ and _requests_.

_Actions_ cause some change within a component and then return the `Unit` value. A constructor for an action will look something like this:

``` purescript
Toggle a
```

Any number of parameters can go before the `a`, but the `a` must be at the end. When we construct the query the `a` will become our `Unit` return type, but we can't fix it as that right now.

_Requests_ can cause changes within the component but also return useful information when evaluated. A constructor for a request will look something like this:

``` purescript
IsOn (Boolean -> a)
```

The callback-looking function we have here also must go at the end of any possible parameters. Additionally it must be exactly `result -> a` where `result` is the value we want as the return type. These request-style queries are also why we need to have a `Type -> Type` kind. Without the type variable we would have no way to define a constructor that will yield values other than `Unit` during query evaluation.

These rules about the definitions may seem rather arbitrary, but they arise from the way query evaluation is implemented.

## Input values

When a component appears as a child of another it can be useful to be able to pass values down to it when the parent re-renders. Our button component needs no input like this, so [`Unit`][Data.Unit.Unit] is used.

More details on this will be covered later, in the chapter on [parent and child components][parent-child-components].

## Output messages

The last type we need to define for our component is for the output messages it can raise during query evaluation.

For this button component, we only have one message to define. We'd like to tell listeners about the button being toggled and what the new state is:

``` purescript
data Message = Toggled Boolean
```

Not every component will have output messages. When that is the case we use [`Void`][Data.Void.Void] for the output type.

## Rendering

Now we have all our types defined we can move on to declaring how the component should appear. The `render` function is called when the component is constructed and whenever the state is modified:

``` purescript
render :: State -> H.ComponentHTML Query
```

Looking at the type we can see that the state is the input, and a [`ComponentHTML`][Halogen.Component.ComponentHTML] value is produced. This is a pure function, so no permanent state changes can take place during `render`.

### The HTML DSL

Halogen provides a HTML DSL (domain specific language) to construct [`ComponentHTML`][Halogen.Component.ComponentHTML] values. The DSL gives us a type-safe way of declaring HTML so that only suitable properties can be applied to each element. The resulting value is not actual HTML, it is a representation that is later applied to the DOM by Halogen.

In the code examples, the `HH` prefix is for elements, `HP` is for properties, and `HE` is for event handlers. These names are the suggested convention to use whenever referring to the [`Halogen.HTML`][Halogen.HTML], [`Halogen.HTML.Properties`][Halogen.HTML.Properties], and [`Halogen.HTML.Events`][Halogen.HTML.Events] modules.

Element functions generally follow this scheme:

```
elementName [ ... properties ... ] [ ... children ... ]
```

There are exceptions for elements that expect no children, in which case the second array is omitted. For every element there is also a version that skips the properties array, usable by affixing an underscore to the name ([`button_`][Halogen.HTML.Elements.button_] instead of [`button`][Halogen.HTML.Elements.button]).

Event handlers go in with the element properties, and are used to feed action queries back to the component:

``` purescript
HH.button
  [ HP.title label
  , HE.onClick (HE.input_ Toggle)
  ]
  [ HH.text label ]
```

The [`input_`][Halogen.HTML.Events.input_] function here is a convenience used whenever the query we want to raise has no arguments. There's also an [`input`][Halogen.HTML.Events.input] helper for when we want to capture the event triggering the handler, or extract some value from it.

## Query evaluation

Our query algebra describes the operations that we can perform on the component, but we have yet to specify what the component should do for each of those cases:

``` purescript
eval :: forall m. Query ~> H.ComponentDSL State Query Message m
```

Note that the `eval` type is not using the normal `->` function arrow, instead it is [`~>`][Data.NaturalTransformation]. This symbol indicates that the function is a [natural transformation][Data.NaturalTransformation] - a mapping between two functors. We can still write this with the normal function arrow:

``` purescript
eval :: forall m a. Query a -> H.ComponentDSL State Query Message m a
```

But using [`~>`][Data.NaturalTransformation] allows us to omit the `a` on both sides, and better expresses what is going on. Using a natural transformation here is what allows us to have request queries with proper return types.

### `HalogenM`

The `ComponentDSL` type is a convenience synonym for another type, [`HalogenM`][Halogen.Query.HalogenM.HalogenM]:

``` purescript
type ComponentDSL s f = HalogenM s f (Const Void) Void
```

We use `ComponentDSL` for components without children - it fills in some type parameters that are not relevant, saving us from writing them out each time.

[`HalogenM`][Halogen.Query.HalogenM.HalogenM] gives us a DSL for performing actions during component evaluation. The most obvious of these is the ability to manipulate the state of a component. There's a [`MonadState`][Control.Monad.State.Class.MonadState] instance that allows us to do this:

- [`get`][Control.Monad.State.Class.get] retrieves the current state value
- [`gets f`][Control.Monad.State.Class.gets] retrieves the state value and applies `f` - generally used to extract a part of the state (for example, `gets _.someProp` when using a record state).
- [`modify f`][Control.Monad.State.Class.modify] updates the stored state value by applying `f` to it.
- [`put`][Control.Monad.State.Class.put] overwrites the entire current state value. Be careful with this!

Halogen re-exports these functions from the main `Halogen` module  for convenience.

There are a range of other useful instances for `HalogenM`, but we'll cover those elsewhere in the guide.

The other capability we'll be making use of in this component is the mechanism to raise messages. There's no general purpose abstraction for this, so we provide a function [`raise`][Halogen.Query.HalogenM.raise]:

``` purescript
raise :: forall s f g p o m. o -> HalogenM s f g p o m Unit
```

`o` is the type variable for the component message type. We don't need to worry about the other type variables here as the rest only appear in the result.

### Evaluating actions

Let's take a look at the `eval` case for an action:

``` purescript
Toggle next -> do
  state <- H.get
  let nextState = not state
  H.put nextState
  H.raise $ Toggled nextState
  pure next
```

Note the result: `pure next`. `next` is what we called the `a`-typed value for the constructor. Didn't we say earlier that `a` is always `Unit` for actions? Well, yes, but we don't have proof that `a ~ Unit` here. We must return something of type `a` since we're in a natural transformation, so `next` is the only `a`-typed value we have to hand. This is also why we can't just use `Unit` or omit the `a` in the constructor definition for an action.

### Evaluating requests

Here's the `eval` case for a request:

``` purescript
IsOn reply -> do
  state <- H.get
  pure (reply state)
```

To pass a result back for a request, we just call the `reply` function we took from the query constructor. This is the `result -> a` function requests must have, as mentioned earlier. One way of looking at this function is it allows us to convert a `Boolean` into an `a`, to satisfy the type of the natural transformation.

## Putting it all together

We have our `State`, `Query`, `Input`, and `Message` types, and `render` and `eval` functions, so what now?

One last thing we'll need is an initial state value, so the component has something to render when first constructed. Let's assume the button should be off by default:

``` purescript
initialState :: State
initialState = false
```

Now we bundle it all up in a [`ComponentSpec`][Halogen.Component.ComponentSpec] record:

``` purescript
type ComponentSpec h s f i o m =
  { initialState :: i -> s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  , receiver :: i -> Maybe (f Unit)
  }
```

All these type variables may be a little intimidating at first, but they become second nature over time. Wherever they appear in types throughout Halogen they always have the same order to make things predictable.

- `h` is the type of value that will be rendered by the component. This is always [`HH.HTML`][Halogen.HTML.Core.HTML] when a component render function uses [`ComponentHTML`][Halogen.Component.ComponentHTML]. The parameter exists to give us the possibility of non-HTML components for things like React Native.
- `s` is the component's state type.
- `f` is the component's query algebra.
- `i` is the type for the component's input values.
- `o` is the type for the component's output messages.
- `m` is a monad used for non-component-state effects (AJAX requests, for example - see [the next chapter][handling-effects]). If a component only manipulates its state, like our button, this should be left as a type variable.

As this component does not use the input value we use a `const initialState` and `const Nothing` in the record for values relating to it:

``` purescript
{ initialState: const initialState
, render
, eval
, receiver: const Nothing
}
```

When a component does use inputs, we can construct the initial state from the input value the component is provided with. The `receiver` is a way of mapping input values to queries. As mentioned earlier, input values will be covered properly in [a later chapter][parent-child-components].

Now we pass our component spec to [`component`][Halogen.Component.component-1], and we're done:

``` purescript
component :: forall h s f o m. ComponentSpec h s f o m -> Component h f o m
```

The resulting [`Component`][Halogen.Component.Component] can now be used [as a child of another component][parent-child-components] or can be [run to produce a UI][running-components].

Next up, let's take a look at how to create a component that can [do something effectful][handling-effects] other than update its own state.

[Control.Monad.State.Class.get]: https://pursuit.purescript.org/packages/purescript-transformers/2.2.0/docs/Control.Monad.State.Class#v:get "Control.Monad.State.Class.get"
[Control.Monad.State.Class.gets]: https://pursuit.purescript.org/packages/purescript-transformers/2.2.0/docs/Control.Monad.State.Class#v:gets "Control.Monad.State.Class.gets"
[Control.Monad.State.Class.modify]: https://pursuit.purescript.org/packages/purescript-transformers/2.2.0/docs/Control.Monad.State.Class#v:modify "Control.Monad.State.Class.modify"
[Control.Monad.State.Class.MonadState]: https://pursuit.purescript.org/packages/purescript-transformers/2.2.0/docs/Control.Monad.State.Class#t:MonadState "Control.Monad.State.Class.MonadState"
[Control.Monad.State.Class.put]: https://pursuit.purescript.org/packages/purescript-transformers/2.2.0/docs/Control.Monad.State.Class#v:put "Control.Monad.State.Class.put"
[Data.NaturalTransformation]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.NaturalTransformation "Data.NaturalTransformation"
[Data.Void.Void]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.Void#t:Void "Data.Void.Void"
[Data.Unit.Unit]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.Unit#t:Unit "Data.Unit.Unit"
[Halogen.Component.component-1]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#v:component "Halogen.Component.component"
[Halogen.Component.Component]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#t:Component "Halogen.Component.Component"
[Halogen.Component.ComponentHTML]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#t:ComponentHTML "Halogen.Component.ComponentHTML"
[Halogen.Component.ComponentSpec]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#t:ComponentSpec "Halogen.Component.ComponentSpec"
[Halogen.HTML.Core.HTML]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Core#t:HTML "Halogen.HTML.Core.HTML"
[Halogen.HTML.Elements.button_]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Elements#v:button_ "Halogen.HTML.Elements.button_"
[Halogen.HTML.Elements.button]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Elements#v:button "Halogen.HTML.Elements.button"
[Halogen.HTML.Events.input_]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Events#v:input_ "Halogen.HTML.Events.input_"
[Halogen.HTML.Events.input]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Events#v:input "Halogen.HTML.Events.input"
[Halogen.HTML.Events]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Events "Halogen.HTML.Events"
[Halogen.HTML.Properties]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML.Properties "Halogen.HTML.Properties"
[Halogen.HTML]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML "Halogen.HTML"
[Halogen.Query.HalogenM.HalogenM]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query.HalogenM#t:HalogenM "Halogen.Query.HalogenM.HalogenM"
[Halogen.Query.HalogenM.raise]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query.HalogenM#v:raise "Halogen.Query.HalogenM.raise"

[parent-child-components]: 5%20-%20Parent%20and%20child%20components.md "Parent and child components"
[handling-effects]: 3%20-%20Handling%20effects.md "Handling effects"
[running-components]: 4%20-%20Running%20a%20component.md "Running a component"
