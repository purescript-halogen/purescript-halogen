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

[Control.Coroutine.consumer]: https://pursuit.purescript.org/packages/purescript-coroutines/3.1.0/docs/Control.Coroutine#v:consumer "Control.Coroutine.consumer"
[purescript-coroutines]: # "purescript-coroutines"
[virtual-dom]: https://github.com/Matt-Esch/virtual-dom
[example-driver-routing]: # "Routing example"
[example-driver-websockets]: # "WebSockets example"
[Halogen.Aff.Effects.HalogenEffects]: # "Halogen.Aff.Effects.HalogenEffects"
[Halogen.Component.hoist]: # "Halogen.Component.hoist"
[Halogen.VirtualDOM.Driver.runUI]: # "Halogen.VirtualDOM.Driver.runUI"
[Halogen.VirtualDOM.Driver]: # "Halogen.VirtualDOM.Driver"
