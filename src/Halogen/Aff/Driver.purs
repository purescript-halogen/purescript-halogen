module Halogen.Aff.Driver
  ( runUI
  , hydrateUI
  , module Halogen.Aff.Driver.RenderImplementation
  , module Halogen
  ) where

import Prelude (bind, pure, ($), (=<<))

import Data.Map as M
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.RenderImplementation (RenderSpec)
import Halogen.Aff.Driver.RenderImplementation as RenderImplementation
import Halogen.Aff.Driver.HydrationImplementation as HydrationImplementation
import Halogen.Aff.Driver.State (unDriverStateX)
import Halogen.Component (Component)
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM

hydrateUI
  :: forall h r f i o
   . RenderSpec h r
  -> Component h f i o Aff
  -> i
  -> DOM.Node
  -> Aff (HalogenIO f o Aff)
hydrateUI renderSpec component i rootNode = do
  lchs <- liftEffect RenderImplementation.newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    listeners <- Ref.new M.empty
    dsx <- Ref.read =<< HydrationImplementation.runComponentHydrate renderSpec true rootNode lchs (RenderImplementation.rootHandler listeners) i component
    unDriverStateX (\st ->
      pure
        { query: RenderImplementation.evalDriver renderSpec disposed st.selfRef
        , subscribe: RenderImplementation.subscribe fresh listeners
        , dispose: RenderImplementation.dispose renderSpec disposed lchs dsx listeners
        }) dsx

runUI
  :: forall h r f i o
   . RenderSpec h r
  -> Component h f i o Aff
  -> i
  -> Aff (HalogenIO f o Aff)
runUI renderSpec component i = do
  lchs <- liftEffect RenderImplementation.newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    listeners <- Ref.new M.empty
    dsx <- Ref.read =<< RenderImplementation.runComponent renderSpec true lchs (RenderImplementation.rootHandler listeners) i component
    unDriverStateX (\st ->
      pure
        { query: RenderImplementation.evalDriver renderSpec disposed st.selfRef
        , subscribe: RenderImplementation.subscribe fresh listeners
        , dispose: RenderImplementation.dispose renderSpec disposed lchs dsx listeners
        }) dsx
