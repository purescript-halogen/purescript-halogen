module Halogen.Aff.Driver
  ( runUI
  , hydrateUI
  , module Halogen.Aff.Driver.Implementation
  , module Halogen
  ) where

import Debug.Trace
import Prelude

import Control.Coroutine as CR
import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.Either (Either(..), either)
import Data.List ((:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, killFiber, launchAff_, runAff_, try)
import Effect.Aff.AVar as AV
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw, throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.Implementation (RenderSpec)
import Halogen.Aff.Driver.Implementation as Implementation
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Web.DOM.Element (Element) as DOM

hydrateUI
  :: forall h r f i o
   . RenderSpec h r
  -> Component h f i o Aff
  -> DOM.Element
  -> i
  -> Aff (HalogenIO f o Aff)
hydrateUI renderSpec rootElement component i = do
  lchs <- liftEffect Implementation.newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    listeners <- Ref.new M.empty
    dsx <- Ref.read =<< Implementation.runComponentHydrate renderSpec rootElement lchs (Implementation.rootHandler listeners) i component
    unDriverStateX (\st ->
      pure
        { query: Implementation.evalDriver renderSpec disposed st.selfRef
        , subscribe: Implementation.subscribe fresh listeners
        , dispose: Implementation.dispose renderSpec disposed lchs dsx listeners
        }) dsx

runUI
  :: forall h r f i o
   . RenderSpec h r
  -> Component h f i o Aff
  -> i
  -> Aff (HalogenIO f o Aff)
runUI renderSpec component i = do
  lchs <- liftEffect Implementation.newLifecycleHandlers
  fresh <- liftEffect $ Ref.new 0
  disposed <- liftEffect $ Ref.new false
  Eval.handleLifecycle lchs do
    listeners <- Ref.new M.empty
    dsx <- Ref.read =<< Implementation.runComponent renderSpec lchs (Implementation.rootHandler listeners) i component
    unDriverStateX (\st ->
      pure
        { query: Implementation.evalDriver renderSpec disposed st.selfRef
        , subscribe: Implementation.subscribe fresh listeners
        , dispose: Implementation.dispose renderSpec disposed lchs dsx listeners
        }) dsx
