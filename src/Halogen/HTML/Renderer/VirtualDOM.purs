module Halogen.HTML.Renderer.VirtualDOM 
  ( renderHTML
  ) where
      
import Data.Array (map)
import Data.Function    
import Data.Foldable (for_)
import Data.Monoid

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H

import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler
import Halogen.Internal.VirtualDOM
      
newtype Attr i = Attr (forall eff. (i -> Eff eff Unit) -> Props)

runAttr :: forall i eff. (i -> Eff eff Unit) -> Attr i -> Props
runAttr k (Attr f) = f k
  
instance attrRepr :: H.AttrRepr Attr where
  emptyAttr = Attr \_ -> emptyProps
  combineAttr (Attr f) (Attr g) = Attr \k -> f k <> g k 
    
  attr_ key value = Attr \_ -> 
    runFn2 prop (H.runAttributeName key) value
  handler_ name f = Attr \k -> 
    runFn2 handlerProp (H.runEventName name) \ev -> do
      m <- unsafeInterleaveEff $ runEventHandler ev (f ev)
      for_ m k
      
newtype HTML p i = HTML (forall eff. (i -> Eff eff Unit) -> (p -> VTree) -> VTree)

runHTML :: forall p i eff. (i -> Eff eff Unit) -> (p -> VTree) -> HTML p i -> VTree
runHTML k1 k2 (HTML f) = f k1 k2

instance htmlRepr :: H.HTMLRepr HTML where
  mapHTML f (HTML build) = HTML \k1 k2 -> build (k1 <<< f) k2
    
  text_ s = HTML \_ _ -> vtext s
  placeholder_ p = HTML \_ f -> f p
  element_ name attrs els = HTML \k f -> vnode (H.runTagName name) (runAttr k (H.runAttr attrs)) (map (runHTML k f) els)

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
-- | The second argument is used to replace placeholder nodes.
renderHTML :: forall p i eff. (i -> Eff eff Unit) -> (p -> VTree) -> H.HTML p i -> VTree
renderHTML k1 k2 html = runHTML k1 k2 (H.runHTML html)