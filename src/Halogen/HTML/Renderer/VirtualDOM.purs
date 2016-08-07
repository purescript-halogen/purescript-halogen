module Halogen.HTML.Renderer.VirtualDOM (renderHTML) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Exists (runExists)
import Data.ExistsR (runExistsR)
import Data.Foldable (foldl, foldMap)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Nullable (toNullable)

import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core (HTML(..), Prop(..), PropF(..), HandlerF(..), runNamespace, runTagName, runPropName, runAttrName, runEventName)
import Halogen.HTML.Events.Handler (runEventHandler)
import Halogen.Internal.VirtualDOM as V

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML
  :: forall p i eff
   . (i -> Eff (HalogenEffects eff) Unit)
  -> HTML p i
  -> V.VTree
renderHTML driver = go
  where
  go = case _ of
    Text s ->
      V.vtext s
    Element ns name props els ->
      V.vnode
        (toNullable $ runNamespace <$> ns)
        (runTagName name)
        (toNullable $ foldl findKey Nothing props)
        (foldMap (renderProp driver) props) (map go els)
    Slot _ ->
      V.vtext ""

renderProp
  :: forall i eff
   . (i -> Eff (HalogenEffects eff) Unit)
  -> Prop i
  -> V.Props
renderProp driver = case _ of
  Prop e ->
    runExists renderPropF e
  Attr ns name value ->
    let attrName = maybe "" (\ns' -> runNamespace ns' <> ":") ns <> runAttrName name
    in runFn2 V.attr attrName value
  Handler e ->
    runExistsR (renderHandlerProp driver) e
  Ref f ->
    V.refProp (driver <<< f)
  _ ->
    mempty

renderPropF :: forall a. PropF a -> V.Props
renderPropF (PropF key value _) = runFn2 V.prop (runPropName key) value

renderHandlerProp
  :: forall i eff a
   . (i -> Eff (HalogenEffects eff) Unit)
  -> HandlerF i a
  -> V.Props
renderHandlerProp driver (HandlerF name k) =
  runFn2 V.handlerProp (runEventName name)
    \ev -> runEventHandler ev (k ev) >>= maybe (pure unit) driver

findKey :: forall i. Maybe String -> Prop i -> Maybe String
findKey _ (Key k) = Just k
findKey r _ = r
