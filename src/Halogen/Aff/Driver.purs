module Halogen.Aff.Driver
  ( runUI
  , hydrateUI
  , module Halogen.Aff.Driver.Implementation.Types
  , module Halogen
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event as Event
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.Implementation.Hydrate as Hydrate
import Halogen.Aff.Driver.Implementation.Render as Render
import Halogen.Aff.Driver.Implementation.Types (RenderSpec, RenderSpecWithHydration)
import Halogen.Aff.Driver.Implementation.Utils as Utils
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, LifecycleHandlers, unDriverStateX)
import Halogen.Component (Component)
import Web.DOM.Node (Node) as DOM

runImplementation
  :: forall r f o
   . RenderSpec r
  -> (Ref.Ref LifecycleHandlers -> Event.EventIO o -> Effect (Ref.Ref (DriverStateX r f o)))
  -> Aff (HalogenIO f o Aff)
runImplementation renderSpec runComponentImplementation = do
  lchs <- liftEffect Utils.newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    eio <- Event.create
    dsx <- Ref.read =<< runComponentImplementation lchs eio
    unDriverStateX (\st ->
      pure
        { query: evalDriver renderSpec disposed st.selfRef
        , messages: eio.event
        , dispose: dispose renderSpec disposed lchs dsx
        }) dsx

evalDriver
  :: forall s f act ps i o r
   . RenderSpec r
  -> Ref Boolean
  -> Ref (DriverState r s f act ps i o)
  -> forall a. (f a -> Aff (Maybe a))
evalDriver renderSpec disposed ref q =
  liftEffect (Ref.read disposed) >>=
    if _
      then pure Nothing
      else Eval.evalQ (Render.render renderSpec true) ref q -- `isRoot` is true because `evalDriver` is used only on root container

dispose
  :: forall f o r
   . RenderSpec r
  -> Ref Boolean
  -> Ref LifecycleHandlers
  -> DriverStateX r f o
  -> Aff Unit
dispose renderSpec disposed lchs dsx = Eval.handleLifecycle lchs do
  Ref.read disposed >>=
    if _
      then pure unit
      else do
        Ref.write true disposed
        Render.finalize renderSpec true lchs dsx
        dsx # unDriverStateX \{ selfRef } -> do
          (DriverState ds) <- liftEffect $ Ref.read selfRef
          for_ ds.rendering renderSpec.dispose

hydrateUI
  :: forall r f i o
   . RenderSpecWithHydration r
  -> Component f i o Aff
  -> i
  -> DOM.Node
  -> Aff (HalogenIO f o Aff)
hydrateUI renderSpecWithHydration component i rootNode = runImplementation renderSpecWithHydration.renderSpec runComponentImplementation
  where
    runComponentImplementation :: Ref.Ref LifecycleHandlers -> Event.EventIO o -> Effect (Ref.Ref (DriverStateX r f o))
    runComponentImplementation lchs eio = Hydrate.runComponentHydrate renderSpecWithHydration true rootNode lchs (liftEffect <<< eio.push) i component

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
