module Main where

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Function
import Data.Foldable (for_)

import Control.Functor (($>))
import Control.Bind
import Control.Monad (when)
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Debug.Trace

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

import qualified Halogen.Themes.Bootstrap3 as B

import Ace
import Ace.Types (EAce(), Editor())

import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session

foreign import createEditorNode
  "function createEditorNode() {\
  \  return document.createElement('div');\
  \}" :: forall eff. Eff (dom :: DOM | eff) HTMLElement

-- | The application state, which stores the current text, and the string most recently 
-- | copied to the clipboard.
data State = State String (Maybe String)

-- | The type of inputs to the component.
data Input = TextCopied String | TextChanged String

-- | Custom attributes
dataAceText :: forall i. String -> A.Attr i
dataAceText = A.attr $ A.attributeName "data-ace-text"

-- | Using `combine` and `mapP` and some `Profunctor` trickery, we can make the types
-- | match what is required by `runUI`.
-- |
-- | `combine` takes a function which renders two `HTML` elements side-by-side, and
-- | combines the `Component`s, taking the sum of the input types.
ui :: forall p m eff. (Applicative m) => Component p m Input Input
ui = render <$> stateful (State "" Nothing) update
  where
  render :: State -> H.HTML _ (m Input)
  render (State text copied) =
    H.div [ A.class_ B.container ]
          [ H.h1_ [ H.text "ace editor" ]
          , H.div_ [ H.p_ [ H.button [ A.classes [ B.btn, B.btnPrimary ]
                                     , A.onClick (A.input_ (TextChanged ""))
                                     ] [ H.text "Clear" ]
                          ]
                   , H.p_ [ H.text (maybe "" ("Text copied: " <>) copied) ]
                   ]
          , H.div [ dataAceText text ] []
          ]
          
  update :: State -> Input -> State
  update (State text _) (TextCopied copied) = State text (Just copied)
  update (State _ copied) (TextChanged text) = State text copied

main = do
  -- A RefVal is used to manage widget state.
  -- In practice, this data store might need to be more complex, mapping component IDs to individual states.
  editorRef <- newRef Nothing    
    
  Tuple node driver <- runUIWith ui (updateAce editorRef)
  
  doc <- document globalWindow
  b <- body doc
  b `appendChild` node
  
  -- Set up the Ace editor
  els <- querySelector "[data-ace-text]" b
  for_ els \el -> do
    -- Setup the Ace editor
    editor <- Ace.editNode el ace
    session <- Editor.getSession editor
    writeRef editorRef (Just editor)
    Editor.setTheme "ace/theme/monokai" editor
    -- Set up event handlers
    Editor.onCopy editor (driver <<< TextCopied)
    Session.onChange session do
      text <- Editor.getValue editor
      driver $ TextChanged text
      
  where 
  updateAce :: RefVal _ -> HTMLElement -> _ -> Eff _ Unit
  updateAce editorRef el _ = do
    doc <- document globalWindow
    b <- body doc
    els <- querySelector "[data-ace-text]" b

    Just editor <- readRef editorRef
    
    for_ els \el -> do
      -- Set the editor's content to the current value
      text <- getAttribute "data-ace-text" el
      current <- Editor.getValue editor
      when (text /= current) $ void $
        Editor.setValue text Nothing editor