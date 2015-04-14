module Example.Ace where

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Function
import Data.Profunctor (lmap)

import Control.Functor (($>))
import Control.Monad.Eff

import DOM

import Debug.Trace

import Halogen
import Halogen.Signal
import Halogen.Component
import Halogen.Internal.VirtualDOM (Widget())

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

-- | The type of inputs to the Ace widget.
data AceInput = ClearText

-- | The type of outputs/events from the Ace widget.
data AceEvent = TextCopied String

-- | The application state, which stores the string most recently copied to the clipboard.
data State = State (Maybe String)

-- | The complete UI is the union of the events from the widget, and from the controller.
-- | The controller generates the inputs needed by the widget as events, and vice versa.
type Input = Either AceInput AceEvent

-- | Using `combine` and `mapP` and some `Profunctor` trickery, we can make the types
-- | match what is required by `runUI`.
-- |
-- | `combine` takes a function which renders two `HTML` elements side-by-side, and
-- | combines the `Component`s, taking the sum of the input types.
ui :: forall m eff. (Applicative m) => Component (Widget (HalogenEffects (ace :: EAce | eff)) Input) m Input Input
ui = mapP (Right <$>) $ lmap swap $ combine render controls aceEditor
  where
  render :: forall a. H.HTML _ a -> H.HTML _ a -> H.HTML _ a
  render c1 c2 = H.div [ A.class_ B.container ] [ H.h1_ [ H.text "ace editor" ], c1, c2 ]

  swap :: forall a b. Either a b -> Either b a
  swap = either Right Left

-- | The UI for the controls.
-- |
-- | The controls consist of a 'Clear' button, and a label which displays the most recently
-- | copied string.
-- |
-- | Note that the input and output/event types are reversed here.
controls :: forall p m eff. (Applicative m) => Component p m AceEvent AceInput
controls = component (render <$> stateful (State Nothing) update)
  where
  render :: State -> H.HTML _ (m AceInput)
  render (State copied) =
    H.div_ [ H.p_ [ H.button [ A.classes [B.btn, B.btnPrimary]
                             , A.onclick (A.input_ ClearText)
                             ] [ H.text "Clear" ]
                  ]
           , H.p_ [ H.text (maybe "" ("Text copied: " <>) copied) ]
           ]

  update :: State -> AceEvent -> State
  update (State _) (TextCopied copied) = State (Just copied)

-- | The Ace editor is represented as a `Component`, created using `Component.widget`.
aceEditor :: forall m eff. (Functor m) => Component (Widget (HalogenEffects (ace :: EAce | eff)) AceEvent) m AceInput AceEvent
aceEditor = widget { name: "AceEditor", id: "editor1", init: init, update: update, destroy: destroy }
  where
  init :: forall eff. (AceEvent -> Eff (ace :: EAce, dom :: DOM | eff) Unit) -> Eff (ace :: EAce, dom :: DOM | eff) { state :: Editor, node :: Node }
  init driver = do
    node <- createEditorNode
    editor <- Ace.editNode node ace
    Editor.setTheme "ace/theme/monokai" editor
    Editor.onCopy editor (driver <<< TextCopied)
    return { state: editor, node: node }

  update :: forall eff. AceInput -> Editor -> Node -> Eff (ace :: EAce, dom :: DOM | eff) (Maybe Node)
  update ClearText editor _ = Editor.setValue "" Nothing editor $> Nothing

  destroy :: forall eff. Editor -> Node -> Eff (ace :: EAce, dom :: DOM | eff) Unit
  destroy editor _ = Editor.destroy editor

main = do
  Tuple node _ <- runUI ui
  appendToBody node
