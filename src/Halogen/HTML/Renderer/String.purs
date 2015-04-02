module Halogen.HTML.Renderer.String 
  ( HTML()
  , Attr()
  
  , renderHTMLToString
  ) where
    
import Data.Maybe
import Data.Array (mapMaybe)
import Data.Function    
import Data.String (joinWith)
import Data.Foldable (foldMap)
import Data.Monoid
import Data.Bifunctor

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import Halogen.HTML.Events.Types 
  
newtype Attr i = Attr (Maybe String)

runAttr :: forall i. Attr i -> Maybe String
runAttr (Attr s) = s

instance functorAttrRepr :: Functor Attr where
  (<$>) f (Attr s) = Attr s
  
instance attrRepr :: A.AttrRepr Attr where
  attr key value = Attr (Just (A.runAttributeName key <> "=\"" <> A.toAttrString key value <> "\""))
  handler name f = Attr Nothing
      
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
    " " <> joinWith " " (mapMaybe (runAttr <<< A.runAttr) attrs) <> 
    ">" <> foldMap runHTML els <> 
    "</" <> H.runTagName name <> ">"

-- | Render a HTML document as a `String`, usually for testing purposes.
-- |
-- | The rank-2 type ensures that placeholders are allowed.
renderHTMLToString :: forall i. (forall p. HTML p i) -> String
renderHTMLToString = runHTML