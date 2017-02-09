# Parent and child components

So far the examples have only concerned a single component, however this will only take us so far before the state and query algebra become unmanageable. The solution to this is to break our app into components that can be composed.

A component that can contain other components is constructed with the [`parentComponent`](Halogen.Component.parentComponent) function:

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

There are a lot of type variables here, but only `g` and `p` are new on top of the [variables used in `ComponentSpec`](2 - Defining a component.md#putting-it-all-together "Defining a component - Putting it all together"). Here's a breakdown of what they all mean:

- `h` is the type of value that will be rendered by the parent component.
- `s` is the parent component's state type.
- `f` is the parent component's query algebra.
- `g` is the query algebra for child components.
- `p` is the "slot address" for child components.
- `i` is the type for the parent component's input values.
- `o` is the type for the parent component's output messages.
- `m` is a monad used for non-component-state effects.

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

This is a somewhat silly example of a container wrapping a button. It counts counts how many times the button has been toggled, and when asked it can check whether the button is on or off. In reality the "ask" part of this would be unnecessary as the container could use the message from the button to track the state changes, but doing this gives us an excuse to illustrate a request query.

## Slot address

The first new element we see defined for this component is the `Slot` type, which corresponds to the `p` ("slot address") parameter of the component. We use values of this type as ids for the child components.

We're only using one button in the container in this example, so the type only admits one value. We could have just used `Unit` here in fact.

If a container has a list of items then this slot type could be based on an `Int` index or `String` name, or some other kind of id value.

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

These type variables have the same meaning as those we listed at the start of the chapter.

When we want to render a child component in the HTML we use the [`slot`](Halogen.HTML.slot) function:

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

The handler function take a message value from the child component's and translates it into a query on the parent component. We can filter the messages by using the `Maybe` return type, so if we're not interested in what the child has to say we can just use `const Nothing`. If the child outputs no messages, using `Void` as its message type, we can use [`absurd`](Data.Void.absurd).

Care should be taken to avoid using the same slot address for multiple child components. The resulting behaviour is undefined... and almost certainly won't be good.

After a component is initialized in a slot, providing a different value for any parameter other than the input value or slot address will have no effect on the child component. Say we were to render the parent using:

``` purescript
HH.slot ButtonSlot Button.myButton unit (HE.input HandleButton)
```

And then on the next render:

``` purescript
HH.slot ButtonSlot AltButton.myAltButton unit (HE.input HandleAltButton)
```

The change in component and handler would have no effect. The child component that was initialized in that slot will be preserved, and the original handler setup will be used when it emits messages.

Changing the slot address value will cause the original component to be destroyed and a new one will be initialized in its place. Changing input values will be covered in a moment, after querying.

## Querying

As with rendering we need to use a different synonym for the result of query evaluation, as compared with a standalone component:

``` purescript
-- Query evaluation for a standalone component
eval :: f ~> H.ComponentDSL s f o m

-- Query evaluation for a parent component
eval :: f ~> H.ParentDSL s f g p o m
```

[`ParentDSL`][Halogen.Component.ParentDSL] is just a name synonym for [`HalogenM`][Halogen.Query.HalogenM], but provided for symmetry with `ComponentHTML`/`ParentHTML`.

Now we have a child component we can query it during `eval`, using the aptly named `query`:

``` purescript
query
  :: forall s f g p o m a
   . Eq p
  => p
  -> g a
  -> HalogenM s f g p o m (Maybe a)
```

We pass it the slot address value for the component we want to query, and the query to send to it. The result here uses a `Maybe` as we have no guarantee that the slot we're querying is present in the current rendered HTML.

In our example we use `query` to check what the current button state is when evaluating `CheckButtonState` for the parent:

``` purescript
CheckButtonState next -> do
  buttonState <- H.query ButtonSlot $ H.request Button.IsOn
  H.modify (_ { buttonState = buttonState })
  pure next
```

As it happens the `buttonState` we're storing in the container component is `Maybe Boolean`, so we didn't have to do anything before storing it here. Often we'll need to handle the `Maybe` first however. A common pattern is to use the `Traversable` instance of `Maybe`, allowing us to write handlers like:

``` purescript
CheckButtonState next -> do
  H.query ButtonSlot (H.request Button.IsOn) >>= for_ \isOn ->
    -- do something with `isOn :: Boolean`
    pure unit
  pure next
```

As well as being able to query children one at a time, we can send a query to all the children of a component at once, using the `queryAll` function:

``` purescript
queryAll
  :: forall s f g p o m a
   . Ord p
  => g a
  -> HalogenM s f g p o m (Map p a)
```

This sends the same query to every child, and then gives us the result back as a map where the keys are slot addresses and the values are the query result for that child.

That covers it for basic parent/child setups: the only differences between a standalone and parent component are the need to define a slot type, some type synonyms with extra parameters, and the ability to query children.

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

...

[Halogen.Component.ParentHTML]: https://pursuit.purescript.org/packages/purescript-halogen/1.0.0/docs/Halogen.Component#t:ParentHTML "Halogen.Component.ParentHTML"
[Halogen.Component.ParentDSL]: https://pursuit.purescript.org/packages/purescript-halogen/1.0.0/docs/Halogen.Component#t:ParentDSL "Halogen.Component.ParentDSL"
[Halogen.Query.HalogenM]: https://pursuit.purescript.org/packages/purescript-halogen/1.0.0/docs/Halogen.Query#t:HalogenM "Halogen.Query.HalogenM"
[Halogen.Component.parentComponent]: https://pursuit.purescript.org/packages/purescript-halogen/1.0.0/docs/Halogen.Component#v:parentComponent "Halogen.Component.parentComponent"
[Halogen.Component.slot]: https://pursuit.purescript.org/packages/purescript-halogen/1.0.0/docs/Halogen.Component#v:slot "Halogen.Component.slot"
