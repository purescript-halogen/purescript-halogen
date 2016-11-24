module Halogen.VirtualDOM.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Data.Maybe (Maybe(..))
import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.Node.Node (appendChild)

import Halogen.Aff.Driver as AD
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (ComponentSlot, Component)
import Halogen.HTML.Core (HTML)
import Halogen.VirtualDOM.Internal as V
import Halogen.VirtualDOM.Renderer (renderHTML)

import Halogen.Aff.Driver (HalogenIO)

type RenderState =
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
    :: forall f g p
     . (forall x. f x -> Eff (HalogenEffects eff) Unit)
    -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Aff (HalogenEffects eff) RenderState)
    -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
    -> AD.ComponentType
    -> Maybe RenderState
    -> Aff (HalogenEffects eff) RenderState
  render handler child html componentType lastRender = do
    vtree <- renderHTML handler (map _.vtree <<< child) html
    node <- liftEff case lastRender of
      Nothing -> do
        newNode <- V.createElement vtree
        appendChild (htmlElementToNode newNode) (htmlElementToNode element)
        pure newNode
      Just r ->
        V.patch (V.diff r.vtree vtree) r.node
    pure { vtree, node }

  renderChild
    :: Int
    -> Maybe RenderState
    -> Aff (HalogenEffects eff) RenderState
  renderChild keyId lastRender =
    liftEff do
      node <- case lastRender of
        Nothing -> V.createElement (V.vtext "")
        Just r -> pure r.node
      pure { vtree: V.widget keyId node, node }
