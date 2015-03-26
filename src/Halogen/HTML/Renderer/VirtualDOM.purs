module Halogen.HTML.Renderer.VirtualDOM 
  ( renderHTML
  ) where
      
import Data.Array (map)
import Data.Function    
import Data.Foldable (for_)
import Data.Monoid
import Data.Bifunctor

import Control.Alt
import Control.Plus

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H

import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler
import Halogen.Internal.VirtualDOM
      
newtype Attr i = Attr (forall eff. (i -> Eff eff Unit) -> Props)

runAttr :: forall i eff. (i -> Eff eff Unit) -> Attr i -> Props
runAttr k (Attr f) = f k

instance functorAttrRepr :: Functor Attr where
  (<$>) f (Attr g) = Attr \k -> g (k <<< f)

instance altAttrRepr :: Alt Attr where
  (<|>) (Attr f) (Attr g) = Attr \k -> f k <> g k 

instance plusAttrRepr :: Plus Attr where
  empty = Attr \_ -> emptyProps
  
instance attrRepr :: H.AttrRepr Attr where    
  attr_ key value = Attr \_ -> 
    runFn2 prop (H.runAttributeName key) value
  handler_ name f = Attr \k -> 
    runFn2 handlerProp (H.runEventName name) \ev -> do
      m <- unsafeInterleaveEff $ runEventHandler ev (f ev)
      for_ m k
      
newtype HTML p i = HTML (forall eff. (i -> Eff eff Unit) -> (p -> Widget eff) -> VTree)

runHTML :: forall p i eff. (i -> Eff eff Unit) -> (p -> Widget eff) -> HTML p i -> VTree
runHTML k1 k2 (HTML f) = f k1 k2

instance bifunctorHTML :: Bifunctor HTML where
  bimap f g (HTML build) = HTML \k1 k2 -> build (k1 <<< g) (k2 <<< f)

instance htmlRepr :: H.HTMLRepr HTML where
  text_ s = HTML \_ _ -> vtext s
  placeholder_ p = HTML \_ f -> vwidget (f p)
  element_ name attrs els = HTML \k f -> vnode (H.runTagName name) (runAttr k (H.runAttr attrs)) (map (runHTML k f) els)

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
-- | The second argument is used to replace placeholder nodes.
renderHTML :: forall p i eff. (i -> Eff eff Unit) -> (p -> Widget eff) -> H.HTML p i -> VTree
renderHTML k1 k2 html = runHTML k1 k2 (H.runHTML html)