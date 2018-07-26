# Defining a component

We'll use a bottom-up approach to explain how to build a Halogen component. Rather than giving you a complete working example immediately and explaining all of the types, we will start with the smaller parts to teach concepts and then tie them together as time goes on into a full working example. Here's the outline we'll be using:
- [Rendering Static HTML](#rendering-static-html)
    - [Rendering HTML](#rendering-html)
    - [Rendering Properties](#rendering-properties)
    - [Rendering CSS](#rendering-css)
- [Adding State - Rendering Dynamic HTML](#adding-state---rendering-dynamic-html)
- [Handling Events](#handling-events)
    - [Basic concepts](#basic-concepts)
    - [Using Information From Events](#using-information-from-events)
- [Our First Halogen Component](#our-first-halogen-component)


# Rendering Static HTML

### Rendering HTML

We'll begin by first showing how to build a static HTML using Halogen's HTML DSL (domain specific language).

Starting small, there will not be any properties, css, or event handling. Rather, we'll just get used to the DSL itself. We'll render the following HTML that would appear in an HTML document's `body` element in Purescript
```html
<div>
    <div>
        <span>This is text in a span!</span>
    <div>
    <button>You can click me, but I don't do anything</button>
</div>
```
In purescipt, we'll use the `render` function to render our HTML. Note: the following type signature is not the real one in Halogen. We'll add more types as we introduce more concepts. In the below example, the `HH` module is used. It is a module that enables us to write any HTML element using the syntax, `HH.elementName_ [ children ]`
```purescript
-- new imports
import Halogen as H
import Halogen.HTML as HH

render :: H.ComponentHTML
render =
  HH.div_
    [ HH.div_
      [ HH.span_ [ HH.text "This is text in a span!" ] ]
    , HH.button_ [ HH.text "You can click me, but I don't do anything." ]
    ]
```

### Rendering Properties

Now, we will add properties to our static HTML. We'll convert the following HTML into purescript:
```html
<div id="top-div">
    <div class="special-div">
        <span class="class1 class2 class3">This is text in a span!</span>
    <div>
    <button type="button">You can click me, but I don't do anything</button>
</div>
```

In the above example, we used the `HH.elementName_` syntax, and that `_` suffix is important. It means that there are no attributes that we wish to add to the rendered HTML. Looking at this in code...
```purescript
-- ... the real syntax is
HH.elementName [ attributes, css, and other properties ] [ children ]

-- Thus,
HH.elementName_ [children]
-- is an abbreviation for
HH.elementName [] [ children]
```

Understanding this, we can now use Halogen's DSL for HTML attributes via this syntax, `HP.attribute value`. There's a few things to be aware of when using this syntax.
- When attribute names are the same as keywords in Purescript, they often have the `_` suffix added to distinguish them from those keywords.
- Some `HP.attribute` functions require a specific type other than what one might expect (e.g. `String`). It may be necessary to write `HP.attribute $ TypeConstructor value`.
```purescript
import Halogen as H
import Halogen.HTML as HH

-- new imports
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP

render :: H.ComponentHTML
render =
  HH.div [ HP.id_ "top-div" ]
    [ HH.div [ HP.class_ $ ClassName "special-div" ]
      [ HH.span
        [ HP.classes [ ClassName "class1", ClassName "class2", ClassName "class3" ] ]
        [ HH.text "This is text in a span!" ]
      ]
    , HH.button [ HP.type_ "button" ]
      [ HH.text "You can click me, but I don't do anything." ]
    ]
```

### Rendering CSS

Now, we'll render the following HTML that includes CSS in Halogen:
```html
<div id="top-div" style="background-color: red;">
    <div class="special-div">
        <span class="class1 class2 class3" style="font-size: 20px; background-color: orange;">This is text in a span!</span>
    <div>
    <button type="button">You can click me, but I don't do anything</button>
</div>
```

Purescript has a library called `purescript-css` that enables one to add correctly-typed inline CSS. However, Halogen cannot use the library as is. Fortunately, the adapter library, `purescript-halogen-css`, allows us to use it within Halogen. We'll need to add `purescript-halogen-css` to our dependencies before continuing from here.

We can now add CSS using this syntax, `CSS.style $ key value`. When we wish to have multiple key-value pairs, we must use the monadic syntax:
```purescript
CSS.style $ do
    key value
    key value
    key value
```
There's a few things to be aware of with the syntax:
- All of the `key` and `value` functions/values are from `purescript-css`, so look there for CSS types rather than `purescript-halogen-css`.
- Some CSS will require a syntax like this: `key $ TypeConstructor value`

```purescript
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP

-- new imports
-- purescript-halogen-css
import Halogen.HTML.CSS (style)

-- purescript-css
import CSS.Background (backgroundColor)
import CSS.Color (red)
import CSS.Size (px)
import CSS.Font (fontSize)

render :: H.ComponentHTML
render =
  HH.div [ HP.id_ "top-div", style $ backgroundColor red ]
    [ HH.div [ HP.class_ $ ClassName "special-div" ]
      [ HH.span
        [ HP.classes [ ClassName "class1", ClassName "class2", ClassName "class3" ]
        , style do
            fontSize $ px 20.0
            backgroundColor orange
        ]
        [ HH.text "This is text in a span!" ]
      ]
    , HH.button [ HP.type_ "button" ]
      [ HH.text "You can click me, but I don't do anything." ]
    ]
```

The above example demonstrates a readability problem when one wants to define multiple style properties for `span`. To get around it, we can define the code elsewhere using `spanStyle`:

```purescript
-- purescript-halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
-- purescript-halogen-css
import Halogen.HTML.CSS (style)
-- purescript-css
import CSS.Background (backgroundColor)
import CSS.Color (red)
import CSS.Size (px)
import CSS.Font (fontSize)

-- new imports
-- purescript-halogen
import Halogen.HTML.Properties (IProp)

render :: H.ComponentHTML
render =
  HH.div [ HP.id_ "top-div", style $ backgroundColor red ]
    [ HH.div [ HP.class_ $ ClassName "special-div" ]
      [ HH.span
        [ HP.classes [ ClassName "class1", ClassName "class2", ClassName "class3" ]
        , spanStyle
        ]
        [ HH.text "This is text in a span!" ]
      ]
    , HH.button [ HP.type_ "button" ]
      [ HH.text "You can click me, but I don't do anything." ]
    ]

    where

    spanStyle :: forall i r. IProp (style :: String | r) i
    spanStyle = style do
      fontSize $ px 20.0
      backgroundColor orange
      -- and any other additional inline CSS here...
```

## Adding State - Rendering Dynamic HTML

Hopefully, you feel comfortable working with Halogen's HTML DSL now. Next, we'll add state to the `render` function and use it to render dynamic HTML. To reduce the "code noise," we will not be using the code from previous examples. Rather, we'll be starting with a fresh example.

We'll start with a simple example. In this example, we'll be creating a button whose text toggles back and forth between "On" and "Off". This example will not yet show how to change the state, so that it actually works. Rather, it demonstrates how would would use the State to create dynamic HTML. We'll also define the `initialState` to help you get familiar with seeing it. It'll become important later.

```purescript
-- purescript-halogen
import Halogen as H
import Halogen.HTML as HH

type State = Boolean

initialState :: State
initialState = true

render :: State -> H.ComponentHTML
render isOn =
  let label = if isOn then "On" else "Off"
  in HH.button_ [ HH.text label ]
```

Let's try a slightly more complicated example. This time, we'll use a `div` to render a list of buttons whose text indicates whether they are on or off.
```purescript
-- purescript-halogen
import Halogen as H
import Halogen.HTML as HH

-- new imports
-- purescript-halogen
import Halogen.HTML.Core (HTML)

type State = Array Boolean

initialState :: State
initialState = [true, false, false, true]

render :: State -> H.ComponentHTML
render array = HH.div_ $ createChildren array

  where

  createChildren :: forall p i. Array Boolean -> Array (HTML p i)
  createChildren array = array <#> (\isOn ->  -- "<#>" == "map function array"
    let label = if isOn then "On" else "Off"
    in HH.button_ [ HH.text label ]
    )
```
As shown above, we can create an HTML's children as long as the return value is `forall p i. Array (HTML p i)`

Now, let's return to our prior example and use this approach to create a list of `span` objects. This time, our state will be a record of the various kinds of state we want to render:
```purescript
-- purescript-halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName, HTML, IProp)
import Halogen.HTML.Properties as HP
-- purescript-halogen-css
import Halogen.HTML.CSS (style)
-- purescript-css
import CSS.Background (backgroundColor)
import CSS.Color (red)
import CSS.Size (px)
import CSS.Font (fontSize)

data SpanInfo =
  SpanInfo Array { clazzes :: Array String, textToRender :: String }

type State = { divId :: String, spans :: SpanInfo }

initialState :: State
initialState =
  { divId: "top-div"
  , spans: SpanInfo
    [ {clazzes: [ "class1", "class2", "class3" ], textToRender: "some text here" }
    , {clazzes: [ "class1", "class2", "class3" ], textToRender: "other text here" }
    , {clazzes: [ "class1", "class2", "class3" ], textToRender: "" }
    ]


render :: State -> H.ComponentHTML
render { divId: topDiv, spans: sInfo } =
  HH.div [ HP.id_ topDiv, style $ backgroundColor red ]
    [ HH.div [ HP.class_ $ ClassName "special-div" ] $ createSpans sInfo
    , HH.button [ HP.type_ "button" ]
      [ HH.text "You can click me, but I don't do anything." ]
    ]

    where

    createSpans :: forall p i. SpanInfo -> Array (HTML p i)
    createSpans (SpanInfo array) = array <#> (\rec ->
      HH.span
        [ HP.classes $ rec.clazzes <#> (\s -> ClassName s)
        , spanStyle
        ]
        [ HH.text rec.text ]
      )

      where

      spanStyle :: forall i r. IProp (style :: String | r) i
      spanStyle = style do
        fontSize $ px 20.0
        backgroundColor orange
```

## Event Handling

### Basic Concepts

We'll return back to our basic button example to show how to add event handling.

This section will introduce four new concepts:
- The `HE.onEventName (HE.input_ QueryConstructor)` syntax
- The `data Query a` type
- The `eval` function
- The `H.get`, `H.put`, and `H.modify_` syntax

The `HE.onEventName (HE.input_ QueryConstructor)` is what we put into an element's array of properties: `button [ HE.onClick (HE.input HandleClick) ] [HH.text "text" ]`. It defines how to map the `Event` type (e.g. `MouseEvent`, `KeyEvent`, etc.) to the `Query a` type
The `Query a` type stores all the information needed to handle the event. In this example, we do not need information from the `MouseEvent`. Rather, we only need to be notified when the mouse is clicked so that we can run some code.
Once the required information has been stored in a type of `Query a`, the `eval` function evaluates the `Query a` type. It's where the actual event handling occurs and enables one to do a number of things:
- State manipulation (e.g. getting/setting/modifying the state)
- Effects (e.g. print to the console, send ajax, etc.)
- Evaluate other `Query a` types

We'll talk about Effects and recursively evaluating Queries in a later section. Fow now, we'll explain how to manipulate the state in the `eval` function. We use the `state <- H.get` syntax to get the state and the `H.put newState` syntax to update it. Whenever we update the state in this way, Halogen will re-render the component using the `render` function.

Here's an example:
```purescript
-- purescript-halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Boolean

initialState :: State
initialState = true

data Query a            -- Stores any required info about event
  = HandleMouseClick a  -- In this case, we don't need any info

-- Now the Query type is needed in our render function
render :: State -> H.ComponentHTML Query
render isOn =
  let label = if isOn then "On" else "Off"
  in HH.button
    [ HE.onClick (HE.input_ HandleMouseClick) ] -- handler syntax
    [ HH.text label ]

{- Note: Ignore the 'm' type for now as it will be explained
     in a later page.

eval :: forall m a. Query a -> H.ComponentDSL State Query m a-}
eval :: forall m.   Query   ~> H.ComponentDSL State Query m
eval HandleMouseClick next = do
   state <- H.get
   let newState = not state
   H.put newState   -- causes component to be re-rendered
   pure next        -- the last line must always be 'pure next'
```

In the `eval` function, we are setting a new state that is based on the old one. Rather than use the `do notation` of `H.get >>= \state -> H.put $ not state`, we could just have used `H.modify_ (\state -> not state)` instead.

Be cautious about using `H.modify_`/`H.put`. Every time one of them is called, it will re-render the state. If the state has not been changed, this leads to needless CPU cycles. In short, avoid needless `H.put`/`H.modify_` calls.

### Using Information From Events

There's a few rules to follow when defining the `Query a` type:
- Each data constructor for `Query a` must always include the `a` somewhere (the convention is to have it at the end): `data Query a = First a | Second a | Third a | Fourth -- This last one is wrong, there is no 'a'`
- When one needs to store additional information, they must include those type(s) somewhere in the data constructor (the convention is to place them between the data constructor and the `a`): `data Query a = One Int a | Two Int Int a | Three Int Int Int a`

Thus, if we wanted to store the mouse event itself in our `Query a` type, we would write:
```purescript
data Query a
  = HandleClick MouseEvent a
```

However, this raises a new problem. We originally wrote `HE.onClick (HE.input_ HandleClick)`. How can we get the actual event, so we can pass it into the `HandleClick` constructor?

The problem lays with `HE.input_`, or more specifically the `_` suffix. Similar to how `HH.div_` omits the element's properties array, `HE.input_` ignores the event and simple creates the `Query a` type. If we want access to that event, we'll need a function of `Event -> Query a`. That's the type that `HE.input` accepts as an argument:
```purescript
data Query a
  = HandleClick MouseEvent a

render :: State -> H.ComponentHTML Query
render isOn =
  let label = if isOn then "On" else "Off"
  in HH.button
    [ HE.onClick (HE.input (\mouseEvent -> HandleMouseClick mouseEvent) ]
    [ HH.text label ]

eval :: forall m. Query ~> H.ComponentDSL State Query m
eval HandleMouseClick mouseEvent next = do
   -- extract information from mouse event and do something with it
   --   (e.g. `let offset = offsetX mouseEvent`)
   pure next
```

## Our First Halogen Component

### The Example

We've introduced Halogen's HTML DSL, the `render` and `eval` functions, and the `State` and `Query a` types. We now have enough understanding to build a basic child component. Parent components, components that can send and receive information to and from child components, will be covered in a later section. In the below example, the term, `P-C`, will indicate something that relates to a parent-child concept. These can be ignored for now:
- `type Input`
- `type Message`

In addition to the above, the `main` function will include other Halogen code that has not yet been explained. It's needed to make this code run. It will produce a single button whose label indicates its state: on or off.

Here's the example:
```purescript
module Main where

import Prelude

-- Imports needed for our main function:
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- Imports needed for our component:
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

data Query a = ToggleState a

-- P-C  {
type Input = Unit
data Message = Void
-- }

-- This is the function that builds our Halogen component
--
-- Notice how the 'm' type from our `eval` function has been moved
-- to this type's definition
myButton :: forall m. H.Component HH.HTML Query Input Message m
myButton =
  H.component   -- defines a child component
    { initialState: const initialState   -- P-C
    , render
    , eval
    , receiver: const Nothing            -- P-C
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
        , HE.onClick (HE.input_ ToggleState)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    ToggleState next -> do
      H.modify_ (\state -> not state)
      pure next

-- Main method that makes the code work:
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myButton unit body
```

### Example Using Meta-Language

```purescript
module Main where

import Prelude

-- Imports needed for our main function:
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- Imports needed for our child component:
import Data.Maybe (Maybe(..))

-- purescript-halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName)

-- purescript-halogen-css
import Halogen.HTML.CSS (style)

-- purescript-css
import CSS.Background (backgroundColor)
import CSS.Color (red)


-- P-C  {
type Input = Unit
data Message = Void
-- }

type State = Boolean
{- or as more commonly seen
type State = { field1 :: Type1
             , field2 :: Type2
             , filed3 :: Type3
             }
-}

data Query a
  = DoSomething a
  | DoSomethingWithTheseArguments Type1 Type2 {- TypeN ... -} a

childComponent :: forall m. H.Component HH.HTML Query Input Message m
childComponent =
  H.component
    { initialState: const initialState   -- P-C
    , render
    , eval
    , receiver: const Nothing            -- P-C
    }
  where

  initialState :: State
  initialState = -- initial state value

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button
        -- attributes
        [ HP.title "title"
        , HP.class_ $ ClassName "class"
        , HP.id_ "id"

        -- CSS
        , CSS.style $ backgroundColor red

        -- Events
        , HE.onClick (HE.input_ DoSomething)
        , HE.onMouseMove (HE.input (\e -> DoSomethingWithTheseArguments e))
        , HE.onMouseEnter (HE.input_ $ DoSomethingWithTheseArguments true 3 2)
        , HE.onMouseLeave (HE.input_ $ DoSomethingWithTheseArguments false 2 3)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    DoSomething next -> do
      -- do something here: state manipulation; effects; etc.
      pure next
    DoSomethingWithTheseArguments n1 n2 next -> do
      -- do something with the two ints
      -- note this code will run when
      --   mouse enters, mouse leaves, and mouse is moving
      --   over the button
      pure next

-- Main method that makes the code work:
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI childComponent unit body
```