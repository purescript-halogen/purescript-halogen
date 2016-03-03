module Halogen.HTML.Renderer.VirtualDOM
  ( renderHTML
  , renderTree
  ) where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.State (State(), runState)
import Control.Monad.State.Class (modify)

import Data.Array (snoc)
import Data.Exists (runExists)
import Data.ExistsR (runExistsR)
import Data.Foldable (foldl, foldMap)
import Data.Function (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Nullable (toNullable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Halogen.Effects (HalogenEffects())
import Halogen.Component.Tree (Tree(), runTree)
import Halogen.HTML.Core (HTML(..), Prop(..), PropF(..), HandlerF(..), runNamespace, runTagName, runPropName, runAttrName, runEventName)
import Halogen.HTML.Events.Handler (runEventHandler)
import qualified Halogen.Internal.VirtualDOM as V

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML :: forall p f eff. (forall i. f i -> Aff (HalogenEffects eff) i) -> HTML p (f Unit) -> V.VTree
renderHTML f = go
  where
  go :: HTML p (f Unit) -> V.VTree
  go (Text s) = V.vtext s
  go (Element ns name props els) =
      let ns' = toNullable $ runNamespace <$> ns
          tag = runTagName name
          key = toNullable $ foldl findKey Nothing props
      in V.vnode ns' tag key (foldMap (renderProp f) props) (map go els)
  go (Slot _) = V.vtext ""

renderTree
  :: forall p f eff
   . (forall i. f i -> Aff (HalogenEffects eff) i)
  -> Tree f p
  -> V.VTree
renderTree f =
  runTree \tree ->
    let go (Text s) = V.vtext s
        go (Slot t) = V.widget t tree.eq (renderTree f)
        go (Element ns name props els) =
          let ns' = toNullable $ runNamespace <$> ns
              tag = runTagName name
              key = toNullable $ foldl findKey Nothing props
          in V.vnode ns' tag key (foldMap (renderProp f) props) (map go els)
    in go tree.html

renderProp :: forall f eff. (forall i. f i -> Aff (HalogenEffects eff) i) -> Prop (f Unit) -> V.Props
renderProp _ (Prop e) = runExists (\(PropF key value _) ->
  runFn2 V.prop (runPropName key) value) e
renderProp _ (Attr ns name value) =
  let attrName = maybe "" (\ns' -> runNamespace ns' <> ":") ns <> runAttrName name
  in runFn2 V.attr attrName value
renderProp dr (Handler e) = runExistsR (\(HandlerF name k) ->
  runFn2 V.handlerProp (runEventName name) \ev -> handleAff $ runEventHandler ev (k ev) >>= maybe (pure unit) dr) e
renderProp dr (Initializer f) = V.initProp (handleAff <<< dr <<< f)
renderProp dr (Finalizer f) = V.finalizerProp (handleAff <<< dr <<< f)
renderProp _ _ = mempty

handleAff :: forall eff a. Aff (HalogenEffects eff) a -> Eff (HalogenEffects eff) Unit
handleAff = runAff throwException (const (pure unit))

findKey :: forall i. Maybe String -> Prop i -> Maybe String
findKey _ (Key k) = Just k
findKey r _ = r
