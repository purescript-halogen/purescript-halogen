module Halogen.HTML.Renderer.VirtualDOM (renderHTML) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Exists (runExists)
import Data.ExistsR (runExistsR)
import Data.Foldable (foldMap)
import Data.Function (runFn2)

import Halogen.Effects (HalogenEffects())
import Halogen.HTML (HTML(..), runTagName)
import Halogen.HTML.Events.Handler (runEventHandler)
import Halogen.HTML.Properties (Prop(..), PropF(..), HandlerF(..), runPropName, runEventName)
import qualified Halogen.Internal.VirtualDOM as V

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML :: forall p f eff. (forall i. f i -> Eff (HalogenEffects eff) i) -> HTML p (f Unit) -> V.VTree
renderHTML f = go
  where
  go (Text s) = V.vtext s
  go (Element name attrs els) = V.vnode (runTagName name) (foldMap (renderAttr f) attrs) (map go els)

renderAttr :: forall f eff. (forall i. f i -> Eff (HalogenEffects eff) i) -> Prop (f Unit) -> V.Props
renderAttr _  (Prop e) = runExists (\(PropF key value _) ->
  runFn2 V.prop (runPropName key) value) e
renderAttr dr (Handler e) = runExistsR (\(HandlerF name k) ->
  runFn2 V.handlerProp (runEventName name) \ev -> runEventHandler ev (k ev) >>= dr) e
renderAttr dr (Initializer i) = V.initProp (dr i)
renderAttr dr (Finalizer i) = V.finalizerProp (dr i)
