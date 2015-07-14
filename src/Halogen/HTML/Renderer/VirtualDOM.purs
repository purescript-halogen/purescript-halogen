module Halogen.HTML.Renderer.VirtualDOM (renderHTML) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.Exists (runExists)
import Data.ExistsR (runExistsR)
import Data.Foldable (foldl, foldMap)
import Data.Function (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Nullable (toNullable)

import Halogen.Effects (HalogenEffects())
import Halogen.HTML.Core (HTML(..), Prop(..), PropF(..), HandlerF(..), runNamespace, runTagName, runPropName, runAttrName, runEventName)
import Halogen.HTML.Events.Handler (runEventHandler)
import qualified Halogen.Internal.VirtualDOM as V

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML :: forall p f eff. (forall i. f i -> Eff (HalogenEffects eff) i) -> HTML p (f Unit) -> V.VTree
renderHTML f = go
  where
  go (Text s) = V.vtext s
  go (Element ns name props els) =
    let ns' = toNullable $ runNamespace <$> ns
        key = toNullable $ foldl findKey Nothing props
        props' = foldMap (renderProp f) props
    in V.vnode ns' (runTagName name) key props' (go <$> els)
  go (Placeholder _) = V.vtext ""

renderProp :: forall f eff. (forall i. f i -> Eff (HalogenEffects eff) i) -> Prop (f Unit) -> V.Props
renderProp _  (Prop e) = runExists (\(PropF key value _) ->
  runFn2 V.prop (runPropName key) value) e
renderProp _  (Attr ns name value) =
  let attrName = maybe "" (\ns' -> runNamespace ns' <> ":") ns <> runAttrName name
  in runFn2 V.attr attrName value
renderProp dr (Handler e) = runExistsR (\(HandlerF name k) ->
  runFn2 V.handlerProp (runEventName name) \ev -> runEventHandler ev (k ev) >>= dr) e
renderProp dr (Initializer f) = V.initProp (dr <<< f)
renderProp dr (Finalizer f) = V.finalizerProp (dr <<< f)
renderProp _ _ = mempty

findKey :: forall i. Maybe String -> Prop i -> Maybe String
findKey _ (Key k) = Just k
findKey r _ = r
