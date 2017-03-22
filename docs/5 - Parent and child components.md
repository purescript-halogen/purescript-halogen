# Parent and child components

So far the examples have only concerned a single component, however this will only take us so far before the state and query algebra becomes unmanageable. The answer is to break our app into components that can be composed.

Let's take a look at a component that uses our button component as a child:

``` purescript
import Prelude
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Button as Button

data Query a
  = HandleButton Button.Message a
  | CheckButtonState a

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

data Slot = ButtonSlot
derive instance eqButtonSlot :: Eq Slot
derive instance ordButtonSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { toggleCount: 0
    , buttonState: Nothing }

  render :: State -> H.ParentHTML Query Button.Query Slot m
  render state =
    HH.div_
      [ HH.slot ButtonSlot Button.myButton unit (HE.input HandleButton)
      , HH.p_
          [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
      , HH.p_
          [ HH.text
              $ "Last time I checked, the button was: "
              <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
              <> ". "
          , HH.button
              [ HE.onClick (HE.input_ CheckButtonState) ]
              [ HH.text "Check now" ]
          ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Void m
  eval = case _ of
    HandleButton (Button.Toggled _) next -> do
      H.modify (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    CheckButtonState next -> do
      buttonState <- H.query ButtonSlot $ H.request Button.IsOn
      H.modify (_ { buttonState = buttonState })
      pure next
```

A runnable version of this is available in the [`components` example](../examples/components/).

This is a somewhat silly example of a container wrapping a button. It counts how many times the button has been toggled, and when asked it can check whether the button is on or off. In reality the "ask" part of this would be unnecessary as the container could use the message from the button to track the state changes, but doing this gives us an excuse to illustrate a request query.

As you can see, things are much the same as with a standalone component, only we're using the `parentComponent` constructor now and there a some new types involved.

## Slot address

The first new element we see defined for this component is the `Slot` type. We use values of this type as the IDs for child components in the rendered HTML. "Slot", "slot address", "slot id" are all used interchangeably to refer to these values.

We're only using one button in the container in this example, so the type only admits one value. Given that, `Unit` would have been an equally suitable option to use here.

When a parent component is being used for displaying a list of items then this slot type could be an `Int` index for the items, or perhaps some `String` key value that each item has.

The type we use must implement instances for the `Ord` and `Eq` classes, but we can make the compiler derive these instances for us:

``` purescript
derive instance eqTickSlot :: Eq Slot
derive instance ordTickSlot :: Ord Slot
```

## Rendering

The `render` for parent components uses a [`ParentHTML`][Halogen.Component.ParentHTML] synonym rather than the `ComponentHTML` we used before. It has some extra parameters:

``` purescript
-- Render for a standalone component
render :: s -> H.ComponentHTML f

-- Render for a parent component
render :: s -> H.ParentHTML f g p m
```

- `f` is the query algebra for the parent component
- `g` is the query algebra for child components
- `p` is the slot address type
- `m` is the effect monad

It may seem a little odd that we have to include `m` here when rendering, since no side effects can occur here. We do need evidence that both parent and child components share the same effect monad type for things to work out though.

When we want to render a child component in the HTML we use the [`slot`][Halogen.HTML.slot] function:

``` purescript
slot
  :: forall f m p i o g
   . p
  -> Component HTML g i o m
  -> i
  -> (o -> Maybe (f Unit))
  -> ParentHTML f g p m
```

We pass it:

- a slot address value
- the child component we want to populate the slot with
- an input value
- a handler for output messages

So for our example, that was:

``` purescript
HH.slot ButtonSlot Button.myButton unit (HE.input HandleButton)
```

We're using:

- the `ButtonSlot` value we created
- the button component
- `unit` for the input value (that's all the button component expects)
- a mapping to the `HandleButton` query for our parent component as the message handler.

The handler function takes a message value from the child component's and translates it into a query on the parent component. We can filter the messages by using the `Maybe` return type, so if we're not interested in what the child has to say we can just use `const Nothing`. If the child outputs no messages, using `Void` as its message type, we can use [`absurd`][Data.Void.absurd].

Care should be taken to avoid using the same slot address for multiple child components. The resulting behaviour is undefined... but almost certainly won't be good. If duplicate slot values are detected a warning message will be logged in the browser console.

After a component is initialized in a slot, providing a different value for any parameter other than the input value or slot address will have no effect on the child component. Say we were to render the parent using:

``` purescript
HH.slot ButtonSlot Button.myButton unit (HE.input HandleButton)
```

And then on the next render:

``` purescript
HH.slot ButtonSlot AltButton.myAltButton unit (HE.input HandleAltButton)
```

The change in component and handler would have no effect. The child component that was initialized in that slot will be preserved, and the original handler setup will be used when it emits messages.

Changing the slot address value will cause the original component to be destroyed and a new one will be initialized in its place with a fresh state.

Changing input values will be covered later in this chapter.

## Querying

As with rendering we need to use a different synonym for the result of query evaluation, as compared with a standalone component:

``` purescript
-- Query evaluation for a standalone component
eval :: f ~> H.ComponentDSL s f o m

-- Query evaluation for a parent component
eval :: f ~> H.ParentDSL s f g p o m
```

[`ParentDSL`][Halogen.Component.ParentDSL] is just a name synonym for [`HalogenM`][Halogen.Query.HalogenM], but provided for symmetry with `ComponentHTML` becoming `ParentHTML`.

As with the `render` function we looked at earlier, the additional `g` and `p` parameters are the query algebra for child components and the slot address type.

Now we have a child component we can query it during `eval`, using the aptly named [`query`][Halogen.Query.query]:

``` purescript
query
  :: forall s f g p o m a
   . Eq p
  => p
  -> g a
  -> HalogenM s f g p o m (Maybe a)
```

We pass it the slot address value for the component we want to query, and the query to send to it. The result here uses a `Maybe` as we have no guarantee that the child component we're querying is present in the current rendered HTML.

In our example we use `query` to check what the current button state is when evaluating `CheckButtonState` for the parent:

``` purescript
CheckButtonState next -> do
  buttonState <- H.query ButtonSlot $ H.request Button.IsOn
  H.modify (_ { buttonState = buttonState })
  pure next
```

As it happens the `buttonState` we're storing in the container component is `Maybe Boolean`, so we didn't have to do anything before storing it here. Often we'll need to handle the `Maybe` first however. A common pattern is to use the `Foldable` instance of `Maybe`, allowing us to write handlers like:

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
  :: forall s f g p o m a
   . Ord p
  => g a
  -> HalogenM s f g p o m (Map p a)
```

This sends the same query to every child, and then gives us the result back as a map where the keys are slot addresses and the values are the query result for that child.

That covers it for basic parent/child setups: the only differences between standalone and parent components are the need to define a slot type, some type synonyms with extra parameters, and the ability to query children.

## Component definition

A component that can contain other components is constructed with the [`parentComponent`][Halogen.Component.parentComponent] function:

``` purescript
type ParentComponentSpec h s f g p i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , receiver :: i -> Maybe (f Unit)
  }

parentComponent
  :: forall h s f g p i o m
   . Ord p
  => ParentComponentSpec h s f g p i o m
  -> Component h f i o m
```

The type signature is a little horrifying with all those type variables, but we've already encountered all of them at this point. It's rare to actually use the `ParentComponentSpec` type too - it's just there as a way of bundling up the arguments we want to pass to `parentComponent`, usually we'd pass the record in immediately.

Nevertheless, here's one more reminder of what they all mean:

- `h` is the type of value that will be rendered by the parent component (`HH.HTML` for "normal" use).
- `s` is the parent component's state type.
- `f` is the parent component's query algebra.
- `g` is the query algebra for child components.
- `p` is the slot address type for child components.
- `i` is the type for the parent component's input values.
- `o` is the type for the parent component's output messages.
- `m` is a monad used for non-component-state effects.

## Input values

So far whenever input values have been mentioned they've been glossed over. Now we know how to embed a child component within a parent we can get into it.

Input values are a means of passing values into a child component every time a parent re-renders. It is also possible to do this by querying the children whenever a parent modifies its state, but as this mechanism is declarative it's less error prone and often more convenient.

First we'll need to set up a component that expects an input:

``` purescript
import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = Int

type State = Int

data Query a = HandleInput Int a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: State
  initialState = 0

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.text "My input value is:"
      , HH.strong_ [ HH.text (show state) ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      when (oldN /= n) $ H.put n
      pure next
```

This is a super simple component that just displays whatever value it has received as its input. As you can see, both the `Input` and `State` type synonyms we have here are `Int`. Usually the component would have some additional private state of its own.

The next part that needs providing is the `receiver` in the component spec. This function determines what to do when provided with an input value. As with output message handlers, it maps the value to a query to raise on the component. Likewise, it is a `i -> Maybe (f Unit)` mapping, so we can choose to do nothing with some input values if we prefer.

Finally, we just need to handle the query in `eval` just as we would with any other query. Input values are triggered every time the parent component renders. This means we might want to perform some check in here to determine whether anything actually needs to happen with the child. In the example above we check to see whether the input value differs from what we have stored in the existing state before doing anything. Calling `put` or `modify` in `eval` will always cause a component to re-render, so by checking whether the state changes first we can prevent unnecessary rendering being done for this component.

When we use this component in a slot now we can pass through an `Int` value based on the parent component's state and see it reflected in the child.

In the [`components-inputs` example](../examples/components-inputs) that the above code was taken from, the "display component" we defined above is used like this:

``` purescript
HH.ul_
  [ HH.slot (Slot 1) Display.component state absurd
  , HH.slot (Slot 2) Display.component (state * 2) absurd
  , HH.slot (Slot 3) Display.component (state * 3) absurd
  , HH.slot (Slot 4) Display.component (state * 10) absurd
  , HH.slot (Slot 5) Display.component (state * state) absurd
  ]
```

This gives us a variety of displays, each with a different permutations of the parent component's state, and all of which update in tandem with changes in the parent component's state.

## Multiple types of child component

The need for multiple types of child component under a parent arises quite often, so Halogen has some tools to help with that.

By "multiple types", an example would be where we have a component for the top level of an app, and inside that view we'd want a menu bar component, a current page view component, a dialog box component, etc. It's unlikely that all these elements would share the same query algebra, so we need a way of dealing with that.

Our parent component only has one one type variable for the child component query algebra and one for the slot types, so how do we do this? By introducing sum types for the various child query algebra and slot types.

It is possible to write your own sum types here, but Halogen offers some convenience functions that should suffice for most cases. We'll be using values from [`Data.Either.Nested`][Data.Either.Nested] for the slot types and [`Data.Functor.Coproduct.Nested`][Data.Functor.Coproduct.Nested] for the query types:

``` purescript
type ChildQuery = Coproduct3 CA.Query CB.Query CC.Query
type ChildSlot = Either3 Unit Unit Unit
```

This and most of the following code snippets are based on the [`components-multitype` example](../examples/components-multitype/).

If you haven't encountered [`Coproduct`][Data.Functor.Coproduct.Coproduct] before, it can be thought of as "either for type constructors" - it expects types of kind `Type -> Type` rather than `Type` for its parameters.

Each position in the nested `Either` should correspond to a position in the nested `Coproduct`. Here we are just using `Unit` for each of the slots since the example only uses one instance of each child component, but in other cases it might look something like:

``` purescript
type ChildSlot = Either3 SlotA SlotB SlotC
```

### Rendering

Rendering is much the same as before, although now we use a variation on the `slot` function when embedding child components:

``` purescript
render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
render state = HH.div_
  [ HH.slot' CP.cp1 unit CA.component unit absurd
  , HH.slot' CP.cp2 unit CB.component unit absurd
  , HH.slot' CP.cp3 unit CC.component unit absurd
  , HH.p_
      [ HH.text "Last observed states:"]
  , HH.ul_
      [ HH.li_ [ HH.text ("Component A: " <> show state.a) ]
      , HH.li_ [ HH.text ("Component B: " <> show state.b) ]
      , HH.li_ [ HH.text ("Component C: " <> show state.c) ]
      ]
  , HH.button
      [ HE.onClick (HE.input_ ReadStates) ]
      [ HH.text "Check states now" ]
  ]
```

Now we're using the [`slot'`][Halogen.HTML.slot'] function we also pass a `cpN` value through. The `N` corresponds to the position of the query/slot in the sum for that type of child component.

These `cpN` functions are provided in [`Halogen.Component.ChildPath`][Halogen.Component.ChildPath] and range from [`cp1`][Halogen.Component.ChildPath.cp1] to [`cp10`][Halogen.Component.ChildPath.cp10].

It's important to note that we want to use [`Either2`][Data.Either.Nested.Either2] and [`Coproduct2`][Data.Functor.Coproduct.Nested.Coproduct2] rather than `Either` and `Coproduct` when there are only two types of child component. Even though they have the same arity, their structure is a little different. There is an alternative notation for these nested types that makes the differences more apparent:

``` purescript
type ChildQuery = CA.Query <\/> CB.Query <\/> CC.Query <\/> Const Void
type ChildSlot = Unit \/ Unit \/ Unit \/ Void
```

These definitions are identical to those we saw earlier, but taking advantage of the type-level operators for `Coproduct` and `Either`. Note how in both cases there is a terminal element based on `Void` - this is like the `Nil` in a list, but done at the type level. The types need this terminal element for the `cpN` combinators to be able to reference each of the other positions.

### Querying

As with `slot` being replaced with `slot'` in the rendering, we use [`query'`][Halogen.Query.query'] with a `cpN` value instead of `query` in `eval`:

``` purescript
eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
eval (ReadStates next) = do
  a <- H.query' CP.cp1 unit (H.request CA.GetState)
  b <- H.query' CP.cp2 unit (H.request CB.GetCount)
  c <- H.query' CP.cp3 unit (H.request CC.GetValue)
  H.put { a, b, c }
  pure next
```

There is also a [`queryAll'`][Halogen.Query.queryAll'] function that acts like `queryAll`, but takes a [`ChildPath`][Halogen.Component.ChildPath.ChildPath] to filter by child component type:

``` purescript
queryAll'
  :: forall s f g g' p p' o m a
   . (Ord p, Eq p')
  => ChildPath g g' p p'
  -> g a
  -> HalogenM s f g' p' o m (M.Map p a)
```

One final suggestion that may be helpful - instead of using the `cpN` functions directly, aliasing them with more memorable names can be beneficial. A hypothetical example:

``` purescript
cpMenu :: ChildPath MenuQuery ChildQuery MenuSlot ChildSlot
cpMenu = cp1

cpView :: ChildPath ViewQuery ChildQuery ViewSlot ChildSlot
cpView = cp2

cpDialog :: ChildPath DialogQuery ChildQuery DialogSlot ChildSlot
cpDialog = cp3

cpNotification :: ChildPath NoteQuery ChildQuery NoteSlot ChildSlot
cpNotification = cp4

-- etc...
```

This way you don't need to remember the index of each child when writing the rendering/eval code for a component, and can use the names instead.

### Custom `ChildPath` definitions

If for some reason 10 variations in child component isn't enough, it is possible to write your own sum types and corresponding `ChildPath` values. If you have a `Coproduct` / `Either` based setup then it is possible to construct your own paths with the [`cpL`][Halogen.Component.ChildPath.cpL] and [`cpR`][Halogen.Component.ChildPath.cpR] functions and the [`:>`][Halogen.Component.ChildPath.compose] operator that composes `ChildPath`s. For example:

``` purescript
myPath :: forall f g h i p q r s. ChildPath f (Coproduct (Coproduct (Coproduct g f) h) i) p (Either (Either (Either q p) r) s)
myPath = cpL :> cpL :> cpR
```

These functions are also useful if your sum doesn't follow the default nesting order for some reason, as the above path illustrates.

When using `cpL` and `cpR`, `cp2` is equivalent to `cpR :> cpL`, `cp3` is equivalent to `cpR :> cpR :> cpL`, and so on.

If you want to define your own sum types entirely, then this is also possible, as a `ChildPath` is just a container of two [`Prism'`][Data.Lens.Types.Prism'] values from [`purescript-profunctor-lenses`][purescript-profunctor-lenses]. The first prism is for the query algebra, the second for the slot address. Lenses are a bit beyond the scope of this guide, but essentially you can build these prisms using the [`prism'`][Data.Lens.Prism.prism'] function.

You've made it to the end of the guide, as it stands... happy Halogen-ing!

[purescript-profunctor-lenses]: https://github.com/purescript-contrib/purescript-profunctor-lenses

[Data.Either.Nested]: https://pursuit.purescript.org/packages/purescript-either/2.1.0/docs/Data.Either.Nested "Data.Either.Nested"
[Data.Either.Nested.Either2]: https://pursuit.purescript.org/packages/purescript-either/2.1.0/docs/Data.Either.Nested#t:Either2 "Data.Either.Nested.Either2"
[Data.Functor.Coproduct.Coproduct]: https://pursuit.purescript.org/packages/purescript-functors/1.1.0/docs/Data.Functor.Coproduct#t:Coproduct "Data.Functor.Coproduct.Coproduct"
[Data.Functor.Coproduct.Nested]: https://pursuit.purescript.org/packages/purescript-functors/1.1.0/docs/Data.Functor.Coproduct.Nested "Data.Functor.Coproduct.Nested"
[Data.Functor.Coproduct.Nested.Coproduct2]: https://pursuit.purescript.org/packages/purescript-functors/1.1.0/docs/Data.Functor.Coproduct.Nested#t:Coproduct2 "Data.Functor.Coproduct.Nested.Coproduct2"
[Data.Lens.Prism.prism']: https://pursuit.purescript.org/packages/purescript-profunctor-lenses/2.6.0/docs/Data.Lens.Prism#v:prism' "Data.Lens.Prism.prism'"
[Data.Lens.Types.Prism']: https://pursuit.purescript.org/packages/purescript-profunctor-lenses/2.6.0/docs/Data.Lens.Types#t:Prism' "Data.Lens.Types.Prism'"
[Halogen.Component.ChildPath.ChildPath]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath#t:ChildPath "Halogen.Component.ChildPath.ChildPath"
[Halogen.Component.ChildPath.compose]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath#v:compose "Halogen.Component.ChildPath.compose"
[Halogen.Component.ChildPath.cp1]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath#v:cp1 "Halogen.Component.ChildPath.cp1"
[Halogen.Component.ChildPath.cp10]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath#v:cp10 "Halogen.Component.ChildPath.cp10"
[Halogen.Component.ChildPath.cpL]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath#v:cpL "Halogen.Component.ChildPath.cpL"
[Halogen.Component.ChildPath.cpR]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath#v:cpR "Halogen.Component.ChildPath.cpR"
[Halogen.Component.ChildPath]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component.ChildPath "Halogen.Component.ChildPath"
[Halogen.Component.parentComponent]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#v:parentComponent "Halogen.Component.parentComponent"
[Halogen.Component.ParentDSL]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#t:ParentDSL "Halogen.Component.ParentDSL"
[Halogen.Component.ParentHTML]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Component#t:ParentHTML "Halogen.Component.ParentHTML"
[Halogen.HTML.slot']: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML#v:slot' "Halogen.HTML.slot'"
[Halogen.HTML.slot]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.HTML#v:slot "Halogen.HTML.slot"
[Halogen.Query.HalogenM]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query#t:HalogenM "Halogen.Query.HalogenM"
[Halogen.Query.query']: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query#v:query' "Halogen.Query.query'"
[Halogen.Query.query]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query#v:query "Halogen.Query.query"
[Halogen.Query.queryAll']: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query#v:queryAll' "Halogen.Query.queryAll'"
[Halogen.Query.queryAll]: https://pursuit.purescript.org/packages/purescript-halogen/1.2.1/docs/Halogen.Query#v:queryAll "Halogen.Query.queryAll"
[Data.Void.absurd]: https://pursuit.purescript.org/packages/purescript-prelude/2.4.0/docs/Data.Void#v:absurd "Data.Void.absurd"
