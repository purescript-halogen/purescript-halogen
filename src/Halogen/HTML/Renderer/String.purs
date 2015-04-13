module Halogen.HTML.Renderer.String 
  ( renderHTMLToString
  ) where
    
import Data.Maybe
import Data.Array (mapMaybe)
import Data.Function    
import Data.String (joinWith)
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Exists
import Data.Void

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import Halogen.HTML.Events.Types 

renderAttr :: forall i. A.Attr i -> Maybe String
renderAttr (A.Attr e) = runExists (\(A.AttrF f key value) -> Just $ A.runAttributeName key <> "=\"" <> f key value <> "\"") e
renderAttr _ = Nothing

-- | Render a HTML document as a `String`, usually for testing purposes.
renderHTMLToString :: forall i. H.HTML Void i -> String
renderHTMLToString (H.Text s) = s
renderHTMLToString (H.Element name attrs els) =
  "<" <> H.runTagName name <> 
  " " <> joinWith " " (mapMaybe renderAttr attrs) <> 
  ">" <> foldMap renderHTMLToString els <> 
  "</" <> H.runTagName name <> ">"