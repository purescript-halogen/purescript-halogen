module Halogen.VirtualDOM.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, readRef)

import Data.Maybe (Maybe(..))
import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.Node.Node (appendChild)

import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (DriverStateX, unDriverStateX)
import Halogen.Component (ComponentSlot, Component)
import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core (HTML)
import Halogen.VirtualDOM.Internal as V
import Halogen.VirtualDOM.Renderer (renderHTML)

import Halogen.Aff.Driver (HalogenIO)

newtype RenderState s (f :: * -> *) (g :: * -> *) p o (eff :: # !) =
  RenderState
    { node :: HTMLElement
    , vtree :: V.VTree
    }

-- | This function is the main entry point for a Halogen based UI, taking a root
-- | component, initial state, and HTML element to attach the rendered component
-- | to.
-- |
-- | The returned "driver" function can be used to send actions and requests
-- | into the component hierarchy, allowing the outside world to communicate
-- | with the UI.
runUI
  :: forall f eff o
   . Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI component element = AD.runUI (renderSpec element) component

renderSpec :: forall eff. HTMLElement -> AD.RenderSpec HTML RenderState eff
renderSpec element =
  { render
  , renderChild
  }
  where

  render
    :: forall s f g p o
     . (forall x. f x -> Eff (HalogenEffects eff) Unit)
    -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (Ref (DriverStateX HTML RenderState g eff)))
    -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
    -> Maybe (RenderState s f g p o eff)
    -> Eff (HalogenEffects eff) (RenderState s f g p o eff)
  render handler child html lastRender = do
    vtree <- renderHTML handler (getVTree <=< child) html
    node <- case lastRender of
      Nothing -> do
        newNode <- V.createElement vtree
        appendChild (htmlElementToNode newNode) (htmlElementToNode element)
        pure newNode
      Just (RenderState r) ->
        V.patch (V.diff r.vtree vtree) r.node
    pure $ RenderState { vtree, node }

  getVTree :: forall g. Ref (DriverStateX HTML RenderState g eff) -> Eff (HalogenEffects eff) V.VTree
  getVTree ref = readRef ref >>= unDriverStateX \ds ->
    pure case ds.rendering of
      Nothing -> V.vtext ""
      Just (RenderState { vtree }) -> vtree

  renderChild
    :: forall s f g p o
     . Int
    -> Maybe (RenderState s f g p o eff)
    -> Eff (HalogenEffects eff) (RenderState s f g p o eff)
  renderChild keyId lastRender = do
    node <- case lastRender of
      Nothing -> V.createElement (V.vtext "")
      Just (RenderState r) -> pure r.node
    pure $ RenderState { vtree: V.widget keyId node, node }
