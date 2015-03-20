module Halogen.HTML.Renderer.String 
  ( renderHTMLToString
  ) where
    
import Data.Array (map)
import Data.Function    
import Data.String (joinWith)
import Data.Foldable (foldMap)
import Data.Monoid

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H

import Halogen.HTML.Events.Types 
  
newtype Attr i = Attr [String]

runAttr :: forall i. Attr i -> [String]
runAttr (Attr s) = s
  
instance attrRepr :: H.AttrRepr Attr where
  emptyAttr = Attr []
  combineAttr (Attr ss1) (Attr ss2) = Attr (ss1 <> ss2)
  
  attr_ key value = Attr [ H.runAttributeName key <> "=\"" <> show value <> "\"" ]
  handler_ name f = Attr []
      
newtype HTML p i = HTML String

runHTML :: forall p i. HTML p i -> String
runHTML (HTML s) = s

instance htmlRepr :: H.HTMLRepr HTML where
  mapHTML _ (HTML s) = HTML s
  text_ s = HTML s
  placeholder_ _ = HTML "placeholders are not supported"
  element_ name attrs els = HTML $
    "<" <> H.runTagName name <> 
    " " <> joinWith " " (runAttr (H.runAttr attrs)) <> 
    ">" <> foldMap runHTML els <> 
    "</" <> H.runTagName name <> ">"

-- | Render a HTML document as a `String`, usually for testing purposes.
-- |
-- | The rank-2 type ensures that neither events nor placeholders are allowed.
renderHTMLToString :: (forall p i. H.HTML p i) -> String
renderHTMLToString html = runHTML (H.runHTML html)