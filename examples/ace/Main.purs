module Example.Ace where

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
import Halogen.Component
import Halogen.Internal.VirtualDOM (Widget(), widget)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

import qualified Halogen.Themes.Bootstrap3 as B

import Ace
import Ace.Types (EAce(), Editor())

import qualified Ace.Editor as Editor

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Unit

foreign import createEditorNode
  "function createEditorNode() {\
  \  return document.createElement('div');\
  \}" :: forall eff. Eff (dom :: DOM | eff) Node

ui :: forall m node eff. (Applicative m, H.HTMLRepr node) => Component (Widget (HalogenEffects (ace :: EAce | eff))) m node Unit Unit
ui = component (render <$> stateful "" update)
  where
  render :: String -> node _ (m Unit)
  render s = 
    H.div [ A.class_ B.container ]
          [ H.h1_ [ H.text "ace editor" ]
          , H.p_ [ H.button [ A.classes [B.btn, B.btnPrimary]
                            , A.onclick (A.input \_ -> unit) 
                            ] [ H.text "Clear" ] 
                 ]
          , H.p_ [ H.placeholder (makeAceEditor s) ]
          ] 
  
  update :: String -> Unit -> String
  update _ _ = ""
  
-- | Our placeholder will be replaced with a widget, which we will create using the `widget`
-- | function from `Halogen.Internal.VirtualDOM`.
makeAceEditor :: forall eff. String -> Widget (HalogenEffects (ace :: EAce | eff))
makeAceEditor s = widget "AceEditor" "editor1" init update destroy
  where 
  init :: forall eff. Eff (ace :: EAce, dom :: DOM | eff) { state :: Editor, node :: Node }
  init = do
    node <- createEditorNode
    editor <- Ace.editNode node ace
    Editor.setTheme "ace/theme/monokai" editor
    return { state: editor, node: node }
      
  update :: forall eff. Editor -> Node -> Eff (ace :: EAce, dom :: DOM | eff) (Maybe Node)
  update editor node = do
    Editor.setValue s Nothing editor
    return Nothing
  
  destroy :: forall eff. Editor -> Node -> Eff (ace :: EAce, dom :: DOM | eff) Unit
  destroy editor _ = Editor.destroy editor
  
main = do
  Tuple node _ <- runUI ui
  appendToBody node