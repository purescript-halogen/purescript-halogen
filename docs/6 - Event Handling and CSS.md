# Event Handling and CSS

This page will focus specifically on how to handle events and style an element with CSS. We'll return to a slightly modified version of our basic example of a button and add more event handling and CSS to it.

## The original basic button example

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

## Event Handling

The event handling must be defined in our button's `render` function. To exclude as much noise as possible, we're going to omit irrelevant functions and change the Query type to make things as clear as possible.
```purescript
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Event as HE

data Query a
 = DoAction a
 | DoActionThatNeedsMouseEvent MouseEvent a
 | DoActionThatNeedsScreenX Int a

-- state, message, input type declarations
-- component function

render :: State -> H.ComponentHTML Query
render _ =
  HH.button
    [ HE.onClick (HE.input_ DoAction) ]
    [ HH.text "This is the button's text" ]

-- eval function
```

### Breaking down the `onEvent` and `input_` functions

We've seen this kind of code before...
```purescript
HE.onclick (HE.input_ DoAction)
```
but we're going to break down the type signatures of `onClick` and `input_` further to understand them better. Copying from the source:
```purescript
-- implementation omitted
onClick :: forall r i. (MouseEvent -> Maybe i) -> IProp (onClick :: MouseEvent | r) i

input_ :: forall f a. Action f -> a -> Maybe (f Unit)
input_ f _ = Just $ H.action

-- Thus, the line
HE.onClick (HE.input_ DoAction)
-- is the same as the unabbreviated version
HE.onClick (\_ -> Just $ H.action $ DoAction)
```
The `_` suffix means ignore the event since we don't need any information from it. But what if we do want information from that event? Using the unabbreviated version...
```purescript
HE.onClick (\e -> Just $ H.action $ DoActionThatNeedsMouseEvent e)
-- but this gets tedious, which is why we can just use `input`
HE.onClick (HE.input (\e -> DoActionThatNeedsMouseEvent e)

-- input :: forall f a. (a -> Action f) -> a -> Maybe (f Unit)
-- input f event = Just $ action (f event)
```
Most will use the `input_` and `input` functions. However, sometimes one will need to run a handler only when some circumstances are true and ignore it otherwise. In cases like these, the unabbreviated `Just $ H.action $ QueryConstructor` is beneficial:
```purescript
-- An unrelated component example that should only scroll by 20
--  if the click's x coordinate is less than 500
HE.onClick (\e ->
  let
    xScreen = screenX e
  in
    if xScreen < 500 then Just $ H.action $ ScrollBy 20.0 else Nothing
  )
{- Note: the above handling could also be done in the evaluation of the
query. However, I'm not sure what the best practices are here,
so this is described just in case. -}
```

The `input` and `input_` functions work for all the `onEvent` functions.

### Using `stopPropagation` and `preventDefault`

This brings us to the next topic. Some events are not always desired as they may interfere with other event handlers. So, how does one use other Event API, such as `stopPropagation` and `preventDefault`?

If we look at the type signature of these two methods, we can see where they will need to go:
```purescript
stopPropagation          :: Event -> Effect Unit
stopImmediatePropagation :: Event -> Effect Unit
preventDefault           :: Event -> Effect Unit
```
Since each type signature is an `Event -> Effect Unit`, we cannot handle them in the `render` function. Instead, we need to put them in the `eval` function and use `liftEffect`. Recall from "Handling effects" page that when we handle such effects, we need to change the `m` parameter to an either `Effect` or `Aff`. Although `Effect` would work, `Aff` allows us to add additional power later on. So, for this example, we'll use `Aff`.
```purescript
-- from "purescript-web-events"
import Web.Event.Internal.Types (Event)
import Web.Event.Event (stopPropagation, stopImmediatePropagation, preventDefault)

-- from "purescript-web-uievents"
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)
import Web.UIEvent.WheelEvent (WheelEvent, toEvent)

-- from "purescript-aff"
import Effect.Aff (Aff)

-- other Halogen imports

data Query a
  = StopPropagation Event a
  | StopImmediatePropagation Event a
  | PreventDefault Event a
  | DoAllThree Event a

myButton :: {- forall m. -- no longer needed since "m" is now Aff -}
            H.Component HH.HTML Query Input Message {- m -} Aff
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
  render _ =
    HH.button
      [ HE.onClick (HE.input (\e -> StopPropagation $ toEvent e))
      , HE.onMouseEnter (HE.input (\e -> StopImmediatePropagation $ toEvent e))
      , HE.onMouseLeave (HE.input (\e -> PreventDefault $ toEvent e))
      , HE.onWheel (HE.input (\e -> DoAllThree $ toEvent e))
      ]
      [ HH.text "This is the button's text" ]

  eval :: Query ~> H.ComponentDSL State Query Message {- m -} Aff
  eval = case _ of
    StopPropagation e next -> do
      liftEffect $ stopPropagation e
      pure next
    StopPropagation e next -> do
      liftEffect $ stopImmediatePropagation e
      pure next
    StopPropagation e next -> do
      liftEffect $ preventDefault e
      pure next
    DoAllThree e next -> do
      liftEffect $ stopPropagation e
      liftEffect $ stopImmediatePropagation e
      liftEffect $ preventDefault e
      pure next
```

This is boilerplatey, but that's how it works. However, what if one wants to do something with the event after using one of these functions (e.g. `e.preventDefault()`)?

[This solution proposed by thomashoneyman](https://github.com/slamdata/purescript-halogen/issues/426#issuecomment-320390523) has been slightly adapted and updated here:
```purescript
-- imports already mentioned

data Query a
  = DoAction a
  | DoActionWithMouse MouseEvent a
  | PreventDefault Event (Query a)

render :: H.ComponentHTML Query
render =
  HH.button
    [ preventClick (\e -> DoActionWithMouseEvent e) ]
    [ HH.text "Do something" ]

-- where q has a type signature like (MouseEvent -> Query a)
preventClick q =
  HE.onClick \e -> Just $ PreventDefault (toEvent e) $ H.action $ q e

eval = case _ of
  DoAction next -> pure next
  DoActionWithMouse e a ->
    -- do stuff here
    pure next
  PreventDefault ev q -> do
    liftEff $ preventDefault ev
    -- "eval q" will block until the query finishes its evaluation
    -- So, no need to worry about concurrency issues by doing this
    eval q
```

## CSS

To add css to our button via the `class` attribute, we can use the `HP.class_` property:
```purescript
-- for a single class name
HH.button [ HP.class_ $ ClassName "one-class-name" ] []

-- for multiple class names
HH.button [ HP.classes [ ClassName "first", ClassName "second", ... ] []
```

To add inline-CSS to our button, we need to install the [halogen-css](https://github.com/slamdata/purescript-halogen-css) library, which acts as a bridge between Halogen and [purescript-css](https://github.com/slamdata/purescript-css), into our project.

Once setup the syntax is trivial:
```purescript
-- from purescript-css
import CSS.ModuleName1 (key)   -- Sometimes, the keys are grouped with
import CSS.ModuleName2 (value) --   the values. Sometimes, not.
import Halogen.HTML.CSS as CSS

-- other imports

-- in the render function
  HH.button
    [ CSS.style $ do
        key value
        key value
        key value
    ]
    []
```
Or for example...
```purescript
-- from purescript-css
import CSS.Background (backgroundColor)
import CSS.Color (red)
import CSS.Display (absolute, position)
import CSS.Geometry (height, width, left, top)
import CSS.Size (px)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Event as HE

data Query a
 = DoAction a
 | DoActionThatNeedsMouseEvent MouseEvent a
 | DoActionThatNeedsScreenX Int a

render :: State -> H.ComponentHTML Query
render _ =
  HH.button
    [ CSS.style $ do
        position absolute
        left $ px 40.0
        top $ px 80.0
        width $ px 100.0
        height $ px 100.0
        backgroundColor $ red

    ]
    [ HH.text "This is the button's text" ]
```

Since the above style's `do notation` makes things less readable, it helps to define the style elsewhere in another function and then use it in the render function. For example:
```purescript
-- from purescript-css
import CSS.Background (backgroundColor)
import CSS.Color (red)
import CSS.Display (absolute, position)
import CSS.Geometry (height, width, left, top)
import CSS.Size (px)

-- new imports
import Halogen.HTML.Properties (IProp)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Event as HE

-- other component types and functions

render :: State -> H.ComponentHTML Query
render _ =
  HH.button
    [ buttonStyle 40.0 80.0 100.0 100.0 ]
    [ HH.text "This is the button's text" ]

    where
      buttonStyle :: Number -> Number -> Number -> Number
                  -> forall i r. IProp (style :: String | r) i
      buttonStyle x y w h =
        CSS.style $ do
          position absolute
          left $ px x
          top $ px y
          width $ px w
          height $ px h
          backgroundColor $ red
```