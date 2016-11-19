# Defining a component

Here's a somewhat contrived example of a Halogen component:

``` purescript
import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

myButton :: forall m. H.Component HH.HTML Query Message m
myButton = H.component { initialState, render, eval }
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

This is a component for a button that displays its current state as the label, and is toggled when clicked by a user. The rest of this chapter will break down the elements involved here.

## State

For this button component, we just need a simple state type: is the button on or off?

``` purescript
type State = Boolean
```

Often a component's state type will be a synonym for a record, as the state will need to include more elements than we have here.

## Query algebra

The type that specifies the queries for a component is known as the _query algebra_. Query algebras are of kind `* -> *` (meaning they always have a type variable). This type variable allows us to specify the return type of a query when we construct it. Constructors in a query algebra define operations on the component that we call _actions_ and _requests_.

_Actions_ cause some change within a component and then return the `Unit` value. A constructor for an action will look something like this:

``` purescript
Toggle a
```

Any number of parameters can go before the `a`, but the `a` must be at the end. When we construct the query the `a` will become our `Unit` return type, but we can't fix it as that right now.

_Requests_ can cause changes within the component but also return useful information when evaluated. A constructor for a request will look something like this:

``` purescript
IsOn (Boolean -> a)
```

The callback-looking function we have here also must go at the end of any possible parameters. Additionally it must be exactly `result -> a` where `result` is the value we want as the return type.

These rules about the definitions may seem rather arbitrary, but they arise due to the way query evaluation is handled.

## Output messages

The last type we need to define for our component is for the output messages it can raise during query evaluation.

For this button component, we only have one message to define. We'd like to tell listeners about the button being toggled, and what the new state is:

``` purescript
data Message = Toggled Boolean
```

Not every component will have output messages. If this is so, we use the [`Void`][Data.Void] type to express that fact.

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

We have our `State`, `Query`, and `Message` types, and `render` and `eval` functions, so what now?

One last thing we'll need is an initial state value, so the component has something to render when first constructed. Let's assume the button should be off by default:

``` purescript
initialState :: State
initialState = false
```

Now we just bundle it all up in a record and throw it at the [`component`][Halogen.Component.component] function!

The record has a type, [`ComponentSpec`][Halogen.Component.ComponentSpec]:

``` purescript
type ComponentSpec h s f o m =
  { initialState :: s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  }
```

All these type variables may be a little intimidating at first, but they become second nature over time. Wherever they appear in types throughout Halogen they always have the same order to make things predictable.

- `h` is the type of value that will be rendered by the component. This is always [`HH.HTML`][Halogen.HTML.Core.HTML] when a component render function uses [`ComponentHTML`][Halogen.Component.ComponentHTML]. The parameter exists to give us the possibility of non-HTML components for things like React Native.
- `s` is the component's state type.
- `f` is the component's query algebra.
- `o` is the type for the component's output messages.
- `m` is a monad used for non-component-state effects (AJAX requests, for example - see [the next chapter](3 - Handling effects.md "Handling effects")). If a component only manipulates its state, like our button, this should be left as a type variable.

[`component`][Halogen.Component.component] takes this record and give us back an opaque [`Component`][Halogen.Component.Component] type:

``` purescript
component :: forall h s f o m. ComponentSpec h s f o m -> Component h f o m
```

We can now use our component [as a child of another component](#) or [run it to produce a UI](#).

[Control.Monad.State.Class.get]: https://pursuit.purescript.org/packages/purescript-transformers/2.0.2/docs/Control.Monad.State.Class#v:get "Control.Monad.State.Class.get"
[Control.Monad.State.Class.gets]: https://pursuit.purescript.org/packages/purescript-transformers/2.0.2/docs/Control.Monad.State.Class#v:gets "Control.Monad.State.Class.gets"
[Control.Monad.State.Class.modify]: https://pursuit.purescript.org/packages/purescript-transformers/2.0.2/docs/Control.Monad.State.Class#v:modify "Control.Monad.State.Class.modify"
[Control.Monad.State.Class.MonadState]: https://pursuit.purescript.org/packages/purescript-transformers/2.0.2/docs/Control.Monad.State.Class#t:MonadState "Control.Monad.State.Class.MonadState"
[Control.Monad.State.Class.put]: https://pursuit.purescript.org/packages/purescript-transformers/2.0.2/docs/Control.Monad.State.Class#v:put "Control.Monad.State.Class.put"
[Data.NaturalTransformation]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.NaturalTransformation "Data.NaturalTransformation"
[Data.Void]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.Void "Data.Void"
[Halogen.Component.component]: # "Halogen.Component.component"
[Halogen.Component.Component]: # "Halogen.Component.Component"
[Halogen.Component.ComponentHTML]: # "Halogen.Component.ComponentHTML"
[Halogen.Component.ComponentSpec]: # "Halogen.Component.ComponentSpec"
[Halogen.HTML.Core.HTML]: # "Halogen.HTML.Core.HTML"
[Halogen.HTML.Elements.button_]: # "Halogen.HTML.Elements.button_"
[Halogen.HTML.Elements.button]: # "Halogen.HTML.Elements.button"
[Halogen.HTML.Events.input_]: # "Halogen.HTML.Events.input_"
[Halogen.HTML.Events.input]: # "Halogen.HTML.Events.input"
[Halogen.HTML.Events]: # "Halogen.HTML.Events"
[Halogen.HTML.Properties]: # "Halogen.HTML.Properties"
[Halogen.HTML]: # "Halogen.HTML"
[Halogen.Query.HalogenM.HalogenM]: # "Halogen.Query.HalogenM.HalogenM"
[Halogen.Query.HalogenM.raise]: # "Halogen.Query.HalogenM.raise"
