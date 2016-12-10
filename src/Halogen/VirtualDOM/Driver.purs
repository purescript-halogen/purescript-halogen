module Halogen.VirtualDOM.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef)
import Control.Monad.Eff.Class (liftEff)

import Data.Maybe (Maybe(..))
import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.Node.Node (appendChild)

import Halogen.Aff.Driver as AD
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Component (ComponentSlot, Component)
import Halogen.HTML.Core (HTML)
import Halogen.VirtualDOM.Internal as V
import Halogen.VirtualDOM.Renderer (renderHTML)

import Halogen.Aff.Driver (HalogenIO)

newtype RenderState s (f :: * -> *) (g :: * -> *) p o (eff :: # !) =
  RenderState
    { keyId :: Int
    , node :: HTMLElement
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
runUI component element = do
  fresh <- liftEff (newRef 0)
  AD.runUI (mkRenderSpec element fresh) component

mkRenderSpec
  :: forall eff
   . HTMLElement
  -> Ref Int
  -> AD.RenderSpec HTML RenderState eff
mkRenderSpec element fresh =
  { render
  , renderChild
  }
  where

  render
    :: forall s f g p o
     . (forall x. f x -> Eff (HalogenEffects eff) Unit)
    -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX HTML RenderState g eff))
    -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
    -> Maybe (RenderState s f g p o eff)
    -> Eff (HalogenEffects eff) (RenderState s f g p o eff)
  render handler child html lastRender = do
    vtree <- renderHTML handler (map getVTree <<< child) html
    case lastRender of
      Nothing -> do
        modifyRef fresh (_ + 1)
        keyId <- readRef fresh
        node <- V.createElement vtree
        appendChild (htmlElementToNode node) (htmlElementToNode element)
        pure $ RenderState { keyId, vtree, node }
      Just (RenderState r) -> do
        node <- V.patch (V.diff r.vtree vtree) r.node
        pure $ RenderState { keyId: r.keyId, vtree, node }

  getVTree :: forall g. RenderStateX HTML RenderState g eff -> V.VTree
  getVTree = unRenderStateX \(RenderState { vtree }) -> vtree

  renderChild
    :: forall s f g p o
     . RenderState s f g p o eff
    -> RenderState s f g p o eff
  renderChild (RenderState r) =
    RenderState { keyId: r.keyId, vtree: V.widget r.keyId r.node, node: r.node }
