# Running a component

So far we've learned how to define a component, and to use `Aff` for effect handling. For this information to be of any use we're going to want to see something in the browser!

Halogen currently provides a driver based on [`virtual-dom`][virtual-dom]. In the future there may be alternatives, but for now it means we'll be working with the [`Halogen.VirtualDOM.Driver`][Halogen.VirtualDOM.Driver] module.

The most basic possible `main` function for a Halogen app will look something like this:

``` purescript
import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VirtualDOM.Driver (runUI)
import Component (myComponent)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myComponent body
```

This assumes our component is pure, we don't care about any messages it might emit, and have no need to send queries into it.

## `runUI`

The main function involved here is [`runUI`][Halogen.VirtualDOM.Driver.runUI]. It takes a component and a reference to a HTML element to use as a container:

``` purescript
runUI
  :: forall f eff o
   . Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
```

The element we pass in here should already be present in the DOM, and should be empty. If either of these conditions are not met then strange things may occur - the behaviour is unspecified.

We expect the component's `m` type variable to be `Aff (HalogenEffects eff)` at this point, and this is also what `runUI` returns in. This is why in the previous chapter the recommendation was made to use `Aff` for components even if you only need `Eff`. If the component is pure, this type will work out since the `m` type should be a type variable and we can substitute `Aff` in. If we have something else in here, then the component will have to be [`hoist`][Halogen.Component.hoist]ed into `Aff`.

The [`HalogenEffects`][Halogen.Aff.Effects.HalogenEffects] type here is a synonym for the row of effects involved in actually running components:

``` purescript
type HalogenEffects eff =
  ( avar :: AVAR
  , ref :: REF
  , err :: EXCEPTION
  , dom :: DOM
  | eff
  )
```

- `AVAR` and `REF` are both used in the internal component machinery.
- `EXCEPTION`s are possible, but only if you try _really_ hard. They should never occur from operations provided by Halogen itself.
- `DOM`... well, this one is probably self explanatory.

The last thing to look at here is the resulting `HalogenIO` value. It's a record that gives us some options for communicating with the component we just ran:

``` purescript
type HalogenIO f o m =
  { query :: f ~> m
  , subscribe :: Consumer o m Unit -> m Unit
  }
```

Note that `m` is polymorphic in the synonym. It's populated with `Aff (HalogenEffects eff)` once again for our case.

- `query` allows us to send queries into the component, using its query algebra (`f`). This is useful for things like [routing][example-driver-routing], or driving an app from an external source - [WebSockets][example-driver-websockets], for example.
- `subscribe` allows us to receive the messages the component emits by providing a [`coroutine`][purescript-coroutines] `Consumer`.

If we go back to our basic button example from [chapter 2](2 - Defining a component.md "Defining a component"), we can demonstrate both of the above with something like this:

``` purescript
import Prelude
import Control.Coroutine as CR
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VirtualDOM.Driver (runUI)
import Button as B

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.myButton body

  io.subscribe $ CR.consumer \(B.Toggled newState) -> do
    log $ "Button was toggled to: " <> show newState
    pure Nothing

  io.query $ H.action $ B.Toggle
  io.query $ H.action $ B.Toggle
  io.query $ H.action $ B.Toggle
```

Here we're setting up a consumer that will listen to the component forever (as it returns `pure Nothing` - see the docs for [`consumer`][Control.Coroutine.consumer] for an explanation), and immediately toggling the button several times on start up. Checking the browser console should reveal the corresponding logged "Button was toggled" messages.

## `Aff`-based utility functions

Aside from `runUI` we used a couple of other utility functions in our `main`, exported from `Halogen.Aff`:

- [`runHalogenAff`][Halogen.Aff.Util.runHalogenAff] runs a Halogen-produced `Aff` value, turning it into an `Eff` so it can be used as `main` for a PureScript bundle. It's provided as a convenience - there is no special behaviour here that couldn't be implemented with functions provided by [`aff`][purescript-aff].
- [`awaitBody`][Halogen.Aff.Util.awaitBody] fetches the `body` element when the document loads. Since we're in `Aff` we can use this to avoid the need for callbacks. This is used when the entire page is going to be a Halogen app.

There are also two more functions provided for cases where we want to run our Halogen app as just part of the page, rather than embedding it in the `body`:

- [`awaitLoad`][Halogen.Aff.Util.awaitLoad] does what the name suggests - waits for the document to load.
- [`selectElement`][Halogen.Aff.Util.selectElement] is a wrapper around `querySelector` - using this after `awaitLoad` allows targeting of a particular container element on the page, to embed our app within.

Now we know how to build simple components and run them, we can take a look at [embedding child components within a parent](5 - Parent and child components "Parent and child components").

[example-driver-routing]: ../examples/driver-routing "Routing example"
[example-driver-websockets]: ../examples/driver-websockets "WebSockets example"
[purescript-aff]: https://pursuit.purescript.org/packages/purescript-aff "purescript-aff"
[purescript-coroutines]: https://pursuit.purescript.org/packages/purescript-coroutines "purescript-coroutines"
[virtual-dom]: https://github.com/Matt-Esch/virtual-dom

[Control.Coroutine.consumer]: https://pursuit.purescript.org/packages/purescript-coroutines/3.1.0/docs/Control.Coroutine#v:consumer "Control.Coroutine.consumer"
[Halogen.Aff.Effects.HalogenEffects]: # "Halogen.Aff.Effects.HalogenEffects"
[Halogen.Aff.Util.awaitBody]: # "Halogen.Aff.Util.awaitBody"
[Halogen.Aff.Util.awaitLoad]: # "Halogen.Aff.Util.awaitLoad"
[Halogen.Aff.Util.runHalogenAff]: # "Halogen.Aff.Util.runHalogenAff"
[Halogen.Aff.Util.selectElement]: # "Halogen.Aff.Util.selectElement"
[Halogen.Component.hoist]: # "Halogen.Component.hoist"
[Halogen.VirtualDOM.Driver.runUI]: # "Halogen.VirtualDOM.Driver.runUI"
[Halogen.VirtualDOM.Driver]: # "Halogen.VirtualDOM.Driver"
