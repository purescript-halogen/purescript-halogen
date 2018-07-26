# Defining a component

We'll use a bottom-up approach to explain how to build a Halogen component. Rather than giving you a complete working example immediately and explaining all of the types, we will start with the smaller parts to teach concepts and then tie them together as time goes on into a full working example. Here's the outline we'll be using:
- [Static Rendering](#static-rendering) - Start by showing how to render static HTML using Halogen's HTML DSL (domain specific language) that guarantees correctly-written HTML
- [Rendering Properties](#rendering-properties) - Add properties (i.e. attributes) to the static HTML
- [Rendering CSS](#rendering-css) - Add properties (i.e. CSS) to the static HTML
- [Adding State](#adding-state) - Add `State` to our component and make our HTML more dynamic
- [Adding Event Handling](#adding-event-handling) - Add event handling, so that our state gets updated and re-renders the dynamic HTML
- Explain the imports (e.g. `H`, `HH`, `HE`, `HP`, `CSS`) and other Halogen-y stuff


## Static Rendering

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

## Rendering Properties

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

## Rendering CSS

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

## Adding State

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

## Adding Event Handling

We'll return back to our basic button example to show how to add event handling.

This section will introduce four new concepts:
- The `HE.onEventName (HE.input_ QueryConstructor)` syntax
- The `data Query a` type
- The `eval` function

The `HE.onEventName (HE.input_ QueryConstructor)` is what we put into an element's array of properties: `button [ HE.onClick (HE.input HandleClick) ] [HH.text "text" ]`. It defines how to map the `Event` type (e.g. `MouseEvent`, `KeyEvent`, etc.) to the `Query a` type
The `Query a` type stores all the information needed to handle the event. In this example, we do not need information from the `MouseEvent`. Rather, we only need to be notified when the mouse is clicked so that we can run some code.
Once the required information has been stored in a type of `Query a`, the `eval` function evaluates the `Query a` type. It's where the actual event handling occurs and enables one to do a number of things:
- Effects (e.g. print to the console, send ajax, etc.)
- State manipulation (e.g. getting/setting/modifying the state).

Whenever we update the state in the `eval` function, it will re-render the component using the `render` function.

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

{-
Note: Ignore the 'm' type for now. This will be explained later.

eval :: forall m. Query a -> H.ComponentDSL State Query a m -}
eval :: forall m. Query   ~> H.ComponentDSL State Query   m
eval HandleMouseClick next = do
   state <- H.get
   let newState = not state
   H.put newState   -- causes component to be re-rendered
   pure next        -- the last line must always be 'pure next'
```



There's a few rules to follow when defining the `Query a` type:
- Each data constructor must end with the `a` (`data Query a = First a | Second a | Third a | Fourth -- This last one is wrong, there is no 'a'`)
- Any

So, what's going on here? We've defined a new type called `Query a`. The `a` is always `Unit` but it must remained generic for the types to align and work.