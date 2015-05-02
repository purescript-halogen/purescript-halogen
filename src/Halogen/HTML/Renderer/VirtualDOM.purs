module Halogen.HTML.Renderer.VirtualDOM 
  ( renderHTML
  ) where
      
import Data.Void
import Data.Array (map)
import Data.Function    
import Data.Foldable (for_, foldMap)
import Data.Monoid
import Data.Exists

import Control.Monad.Eff
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler
import Halogen.Internal.VirtualDOM
      
renderAttr :: forall i eff. (i -> Eff eff Unit) -> A.Attr i -> Props
renderAttr _  (A.Attr e) = runExists (\(A.AttrF _ key value) -> runFn2 prop (A.runAttributeName key) value) e
renderAttr dr (A.Handler e) = A.runExistsR (\(A.HandlerF name k) ->
  runFn2 handlerProp (A.runEventName name) \ev -> do
    a <- unsafeInterleaveEff $ runEventHandler ev (k ev)
    dr a) e
renderAttr dr (A.Initializer i) = initProp (dr i)
renderAttr dr (A.Finalizer i) = finalizerProp (dr i)

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
-- | The second argument is used to replace placeholder nodes.
renderHTML :: forall i eff. (i -> Eff eff Unit) -> H.HTML Void i -> VTree
renderHTML f = go
  where
  go (H.Text s) = vtext s
  go (H.Element name attrs els) = vnode (H.runTagName name) (foldMap (renderAttr f) attrs) (map go els)