module Halogen.Aff.Driver.Implementation.Hydrate where

import Prelude

import Control.Monad.Fork.Class (fork)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Parallel (parSequence_)
import Data.List (List, (:))
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isJust, isNothing)
import Data.Traversable (for_, sequence_, traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM, trace)
import Effect (Effect)
import Effect.Aff (Aff, killFiber)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event as Event
import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState, DriverStateX, LifecycleHandlers, unDriverStateX)
import Halogen.Aff.Driver.Implementation.Types (RenderSpec, RenderSpecWithHydration)
import Halogen.Aff.Driver.Implementation.Utils as Utils
import Halogen.Aff.Driver.Implementation.Render as Render
import Halogen.Aff.Driver.State (DriverState(..), DriverStateRef(..), DriverStateX, LifecycleHandlers, RenderStateX, DriverStateRec, initDriverState, mapDriverState, renderStateX, renderStateX_, unDriverStateX)
import Halogen.Component (Component, ComponentSlot, ComponentSlotBox, ComponentSlotSpec, unComponent, unComponentSlot)
import Halogen.Data.Slot as Slot
import Halogen.HTML.Core as HC
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.Input (Input)
import Halogen.Query.Input as Input
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM

runComponentHydrate
  :: forall f i o r
   . RenderSpecWithHydration r
  -> Boolean
  -> DOM.Node
  -> Ref LifecycleHandlers
  -> (o -> Aff Unit)
  -> i
  -> Component f i o Aff
  -> Effect (Ref (DriverStateX r f o))
runComponentHydrate renderSpecWithHydration isRoot currentNode lchs handler j = Render.runComponentImplementation renderSpecWithHydration.renderSpec isRoot lchs handler j runRender
  where
    runRender :: DriverStateX r f o -> Effect Unit
    runRender = unDriverStateX (renderHydrate renderSpecWithHydration isRoot currentNode lchs <<< _.selfRef)

renderHydrate
  :: forall s f act ps i o r
   . RenderSpecWithHydration r
  -> Boolean
  -> DOM.Node
  -> Ref LifecycleHandlers
  -> Ref (DriverState r s f act ps i o)
  -> Effect Unit
renderHydrate renderSpecWithHydration isRoot currentNode lchs var = Render.renderImplementation renderSpecWithHydration.renderSpec isRoot lchs var runRender
  where
  runRender :: (Input act -> Aff Unit) -> (act -> Aff Unit) -> DriverStateRec r s f act ps i o -> Effect (r s act ps o)
  runRender handler childHandler ds =
    renderSpecWithHydration.hydrate
      (Eval.handleAff <<< handler)
      (Render.renderChild renderSpecWithHydration.renderSpec lchs childHandler ds.childrenIn ds.childrenOut)
      (renderChildHydrate renderSpecWithHydration lchs childHandler ds.childrenIn ds.childrenOut)
      (ds.component.render ds.state)
      currentNode

renderChildHydrate
  :: forall ps act r
   . RenderSpecWithHydration r
  -> Ref LifecycleHandlers
  -> (act -> Aff Unit)
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> Ref (Slot.SlotStorage ps (DriverStateRef r))
  -> ComponentSlotBox ps Aff act
  -> DOM.Node
  -> Effect (RenderStateX r)
renderChildHydrate renderSpecWithHydration lchs handler childrenInRef childrenOutRef componentSlotBox currentNode = Render.renderChildImplementation renderSpecWithHydration.renderSpec lchs handler childrenInRef childrenOutRef renderWithExistingChildrenState renderNew componentSlotBox
  where
  renderWithExistingChildrenState :: forall query input output. ComponentSlotSpec query input output ps Aff act -> Tuple (DriverStateRef r query output) (Slot.SlotStorage ps (DriverStateRef r)) -> Effect (Ref (DriverStateX r query output))
  renderWithExistingChildrenState _ _ = throw "[renderChildHydrate] you are trying to render a component that already has a state. This should not have happened on hydration phase, because hydration is running only on initial rendering. On subsequent rendering the non-hydration functions are used"

  renderNew :: forall query input output. ComponentSlotSpec query input output ps Aff act -> Effect (Ref (DriverStateX r query output))
  renderNew slot = runComponentHydrate renderSpecWithHydration false currentNode lchs (maybe (pure unit) handler <<< slot.output) slot.input slot.component
