module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Ace.Types (ACE())
import AceComponent (AceState(), AceInput(..), AceEffects(), ace, initAceState)

-- | The application state, which stores the current text in the editor.
type State = { text :: String }

initialState :: State
initialState = { text: "" }

-- | The type of inputs to the component.
data Input a = ClearText a

-- | The slot for the ace component.
data AceSlot = AceSlot

derive instance genericAceSlot :: Generic AceSlot
instance eqAceSlot :: Eq AceSlot where eq = gEq
instance ordAceSlot :: Ord AceSlot where compare = gCompare

-- | The main UI component
ui :: forall eff. ParentComponent State AceState Input AceInput (Aff (AceEffects eff)) AceSlot
ui = parentComponent' render eval peek
  where

  render :: RenderParent State AceState Input AceInput (Aff (AceEffects eff)) AceSlot
  render { text: text } =
    H.div_ [ H.h1_ [ H.text "ace editor" ]
           , H.div_ [ H.p_ [ H.button [ E.onClick (E.input_ ClearText) ]
                                      [ H.text "Clear" ]
                           ]
                    ]
           , H.div_ [ H.slot AceSlot \_ -> { component: ace "test-ace", initialState: initAceState } ]
           , H.p_ [ H.text ("Current text: " ++ text) ]
           ]

  eval :: EvalParent Input State AceState Input AceInput (Aff (AceEffects eff)) AceSlot
  eval (ClearText next) = do
    query AceSlot $ action (ChangeText "")
    pure next

  -- | Peek allows us to observe inputs going to the child components, here
  -- | we're using it to observe when the text is changed inside the ace
  -- | component, and igoring any other inputs.
  peek :: Peek (ChildF AceSlot AceInput) State AceState Input AceInput (Aff (AceEffects eff)) AceSlot
  peek (ChildF _ q) = case q of
    ChangeText text _ -> modify _ { text = text }
    _ -> pure unit

-- | Run the Halogen app.
main :: Eff (HalogenEffects (ace :: ACE, console :: CONSOLE)) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
