module Halogen.HTML.Renderer.String (renderHTML) where

import Prelude

import Data.Array (mapMaybe)
import Data.Exists (runExists)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import Halogen.HTML.Core (HTML(..), Prop(..), PropF(..), runTagName, runNamespace, runAttrName)

-- | Render a HTML document as a `String`, usually for testing purposes.
renderHTML :: forall p i. HTML p i -> String
renderHTML (Text s) = s
renderHTML (Element ns name attrs els) =
  let tagName = maybe "" (\ns' -> runNamespace ns' <> ":") ns <> runTagName name
  in "<" <> tagName <>
     " " <> joinWith " " (mapMaybe renderAttr attrs) <>
     ">" <> foldMap renderHTML els <>
     "</" <> tagName <> ">"
renderHTML (Placeholder _) = ""

-- TODO: attr rendering
renderAttr :: forall i. Prop i -> Maybe String
renderAttr (Prop e) = runExists (\(PropF propName value attr) ->
  (\(Tuple attrName f) -> runAttrName attrName <> "=\"" <> f attrName propName value <> "\"") <$> attr) e
renderAttr _ = Nothing
