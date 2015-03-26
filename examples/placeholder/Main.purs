module Example.Placeholder where

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Function

import Control.Monad.Eff

import DOM

import Debug.Trace

import Halogen
import Halogen.Signal
import Halogen.Internal.VirtualDOM (Widget(), widget)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

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
  "function createParagraph() {\
  \  return document.createElement('p');\
  \}" :: forall eff. Eff (dom :: DOM | eff) Node
  
foreign import setTextContent
  "function setTextContent(node, s) {\
  \  return function() {\
  \    node.textContent = s;\
  \  };\
  \}" :: forall eff. Fn2 Node String (Eff (dom :: DOM | eff) Unit)

-- | Instead of embedding nodes directly, we create a placeholder type,
-- | and insert placeholders into our document.
-- |
-- | This way, we keep our view pure, and can separate out impure code into the
-- | renderer function, below.
data Placeholder = Counter Number

view :: forall r. View Unit Placeholder r
view = render <$> stateful 0 update
  where
  render :: Number -> H.HTML Placeholder (Either Unit r)
  render n = 
    H.div (A.class_ B.container)
          [ H.h1_ [ H.text "placeholder" ]
          , H.p_ [ H.text "This counter is rendered using a placeholder:" ]
          , H.p_ [ H.placeholder (Counter n) ]
          , H.button (A.classes [B.btn, B.btnPrimary] <> A.onclick \_ -> pure (Left unit)) [ H.text "Update" ]
          ] 
  
  update :: Number -> Unit -> Number
  update n _ = n + 1
  
-- | Our placeholder will be replaced with a widget, which we will create using the `widget`
-- | function from `Halogen.Internal.VirtualDOM`.
makeCounter :: forall eff. Number -> Widget (HalogenEffects eff)
makeCounter n = widget "PlaceholderLabel" "label1" init update destroy
  where 
  init :: forall eff. Eff (trace :: Trace, dom :: DOM | eff) Node
  init = do
    trace "Init function called"
    node <- createParagraph
    runFn2 setTextContent node ("Count: " <> show n)
    return node
      
  update :: forall eff. Node -> Eff (trace :: Trace, dom :: DOM | eff) (Maybe Node)
  update node = do
    trace "Update function called"
    runFn2 setTextContent node ("Count: " <> show n)
    return Nothing
  
  destroy :: forall eff. Node -> Eff (trace :: Trace, dom :: DOM | eff) Unit
  destroy _ = trace "Destroy function called"
  
-- | Convert placeholders into `VTrees`.
-- |
-- | In a real application, this function might render a third party component using `widget`.
renderer :: forall eff i. Renderer i Placeholder eff
renderer _ (Counter n) = makeCounter n 
  
ui :: forall eff. UI Unit Placeholder Void eff
ui = { view: view, handler: defaultHandler, renderer: renderer }    
  
main = do
  Tuple node _ <- runUI ui
  appendToBody node
  
