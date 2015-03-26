module Halogen.HTML.Renderer.String 
  ( HTML()
  , Attr()
  
  , renderHTMLToString
  ) where
    
import Data.Array (map)
import Data.Function    
import Data.String (joinWith)
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Bifunctor

import Control.Alt
import Control.Plus

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H

import Halogen.HTML.Events.Types 
  
newtype Attr i = Attr [String]

runAttr :: forall i. Attr i -> [String]
runAttr (Attr s) = s

instance functorAttrRepr :: Functor Attr where
  (<$>) f (Attr ss) = Attr ss
  
instance altAttrRepr :: Alt Attr where
  (<|>) (Attr ss1) (Attr ss2) = Attr (ss1 <> ss2)
  
instance plusAttrRepr :: Plus Attr where
  empty = Attr [] 
  
instance attrRepr :: H.AttrRepr Attr where
  attr key value = Attr [ H.runAttributeName key <> "=\"" <> show value <> "\"" ]
  handler name f = Attr []
      
newtype HTML p i = HTML String

runHTML :: forall p i. HTML p i -> String
runHTML (HTML s) = s

instance bifunctorHTML :: Bifunctor HTML where
  bimap _ _ (HTML s) = HTML s

instance htmlRepr :: H.HTMLRepr HTML where
  text s = HTML s
  placeholder _ = HTML "placeholders are not supported"
  element name attrs els = HTML $
    "<" <> H.runTagName name <> 
    " " <> joinWith " " (runAttr (H.runAttr attrs)) <> 
    ">" <> foldMap runHTML els <> 
    "</" <> H.runTagName name <> ">"

-- | Render a HTML document as a `String`, usually for testing purposes.
-- |
-- | The rank-2 type ensures that neither events nor placeholders are allowed.
renderHTMLToString :: (forall p i. HTML p i) -> String
renderHTMLToString = runHTML