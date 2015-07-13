module Halogen.HTML.Renderer.String (renderHTML) where

import Prelude

import Data.Array (mapMaybe)
import Data.Exists (runExists)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as A

-- | Render a HTML document as a `String`, usually for testing purposes.
renderHTML :: forall p i. H.HTML p i -> String
renderHTML (H.Text s) = s
renderHTML (H.Element name attrs els) =
  "<" <> H.runTagName name <>
  " " <> joinWith " " (mapMaybe renderAttr attrs) <>
  ">" <> foldMap renderHTML els <>
  "</" <> H.runTagName name <> ">"

renderAttr :: forall i. A.Prop i -> Maybe String
renderAttr (A.Prop e) = runExists (\(A.PropF propName value attr) ->
  (\(Tuple attrName f) -> A.runAttrName attrName <> "=\"" <> f attrName propName value <> "\"") <$> attr) e
renderAttr _ = Nothing
