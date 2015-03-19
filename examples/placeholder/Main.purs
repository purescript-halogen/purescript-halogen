module Example.Placeholder where

import Data.Void
import Data.Maybe
import Data.Tuple

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal
import Halogen.Internal.VirtualDOM (VTree(), widget)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import qualified Halogen.Themes.Bootstrap3 as B

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

-- | An example of a function which creates a DOM node to 
-- | be embedded inside a HTML document.
foreign import createParagraph
  "function createParagraph(s) {\
  \  return function() {\
  \    var p = document.createElement('p');\
  \    p.style.color = 'red';\
  \    p.appendChild(document.createTextNode(s));\
  \    return p;\
  \  };\
  \}" :: forall eff. String -> Eff (dom :: DOM | eff) Node

-- | Instead of embedding nodes directly, we create a placeholder type,
-- | and insert placeholders into our document.
-- |
-- | This way, we keep our view pure, and can separate out impure code into the
-- | renderer function, below.
data Placeholder = Placeholder String

view :: forall r. H.HTML Placeholder r
view = H.div (A.class_ B.container)
          [ H.h1_ [ H.text "placeholder" ]
          , H.placeholder (Placeholder "A custom widget will be rendered here.")
          ] 
  
-- | Our placeholder will be replaced with a widget, which we will create using the `widget`
-- | function from `Halogen.Internal.VirtualDOM`.
makeWidget :: String -> VTree  
makeWidget s = widget init update destroy
  where 
  init :: forall eff. Eff (dom :: DOM | eff) Node
  init = createParagraph s
      
  update :: forall eff. Node -> Eff (dom :: DOM | eff) (Maybe Node)
  update _ = pure Nothing
  
  destroy :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
  destroy _ = return unit
  
-- | Convert placeholders into `VTrees`.
-- |
-- | In a real application, this function might render a third party component using `widget`.
renderer :: Placeholder -> VTree
renderer (Placeholder s) = makeWidget s  
  
ui :: forall i eff. UI i Placeholder Void eff
ui = { view: pure view, handler: absurd, renderer: renderer }    
  
main = do
  Tuple node _ <- runUI ui
  appendToBody node
  
