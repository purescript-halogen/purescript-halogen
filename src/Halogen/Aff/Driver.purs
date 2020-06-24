module Halogen.Aff.Driver
  ( runUI
  , hydrateUI
  , module Halogen.Aff.Driver.Implementation.Types
  , module Halogen
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event as Event
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.Implementation.Hydrate as Hydrate
import Halogen.Aff.Driver.Implementation.Render as Render
import Halogen.Aff.Driver.Implementation.Types (RenderSpec)
import Halogen.Aff.Driver.State (DriverStateX, LifecycleHandlers)
import Halogen.Aff.Driver.State (unDriverStateX)
import Halogen.Component (Component)
import Web.DOM.Node (Node) as DOM

runImplementation
  :: forall r f o
   . RenderSpec r
  -> (Ref.Ref LifecycleHandlers -> Event.EventIO o -> Effect (Ref.Ref (DriverStateX r f o)))
  -> Aff (HalogenIO f o Aff)
runImplementation renderSpec runComponentImplementation = do
  lchs <- liftEffect Render.newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    eio <- Event.create
    dsx <- Ref.read =<< runComponentImplementation lchs eio
    unDriverStateX (\st ->
      pure
        { query: Render.evalDriver renderSpec disposed st.selfRef
        , messages: eio.event
        , dispose: Render.dispose renderSpec disposed lchs dsx
        }) dsx

hydrateUI
  :: forall r f i o
   . RenderSpec r
  -> Component f i o Aff
  -> i
  -> DOM.Node
  -> Aff (HalogenIO f o Aff)
hydrateUI renderSpec component i rootNode = runImplementation renderSpec runComponentImplementation
  where
    runComponentImplementation :: Ref.Ref LifecycleHandlers -> Event.EventIO o -> Effect (Ref.Ref (DriverStateX r f o))
    runComponentImplementation lchs eio = Hydrate.runComponentHydrate renderSpec true rootNode lchs (liftEffect <<< eio.push) i component

runUI
  :: forall r f i o
   . RenderSpec r
  -> Component f i o Aff
  -> i
  -> Aff (HalogenIO f o Aff)
runUI renderSpec component i = runImplementation renderSpec runComponentImplementation
  where
    runComponentImplementation :: Ref.Ref LifecycleHandlers -> Event.EventIO o -> Effect (Ref.Ref (DriverStateX r f o))
    runComponentImplementation lchs eio = Render.runComponent renderSpec true lchs (liftEffect <<< eio.push) i component
