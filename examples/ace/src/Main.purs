module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)

import Data.Const (Const())
import Data.Functor (($>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen
import Halogen.Util (appendToBody)
import Halogen.Query.StateF (modify)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

import Ace.Types (ACE())
import AceComponent (AceState(), AceInput(..), AceEffects(), ace, initAceState)

-- | The application state, which stores the current text, and the string most
-- | recently copied to the clipboard.
type State = { text :: String, copied :: Maybe String }

initialState :: State
initialState = { text: "", copied: Nothing }

-- | The type of inputs to the component.
data Input a = ClearText a

-- | The placeholder for the ace component. As it is only going to be used once
-- | in this demo the `Eq` and `Ord` instances are trivial. If the component was
-- | going to be used multiple times these would need a proper implementation
-- | for the individual components to function correctly.
data AcePlaceholder = AcePlaceholder

instance eqAcePlaceholder :: Eq AcePlaceholder where
  eq _ _ = true

instance ordAcePlaceholder :: Ord AcePlaceholder where
  compare _ _ = EQ

-- | The parent component that the ace component will be installed into.
container :: forall p eff. ParentComponentP State AceState Input AceInput (Aff (AceEffects eff)) (ChildF AcePlaceholder AceInput) (Const Void) AcePlaceholder p
container = component' render eval peek
  where

  render :: Render State Input AcePlaceholder
  render { text: text, copied: copied } =
    H.div_ [ H.h1_ [ H.text "ace editor" ]
           , H.div_ [ H.p_ [ H.button [ E.onClick (E.input_ ClearText) ]
                                      [ H.text "Clear" ]
                           ]
                    ]
           , H.div_ [ H.Placeholder AcePlaceholder ]
           , H.p_ [ H.text ("Current text: " ++ text) ]
           , H.p_ [ H.text ("Copied text: " ++ fromMaybe "" copied) ]
           ]

  eval :: Eval Input State Input (QueryF State AceState AceInput (Aff (AceEffects eff)) AcePlaceholder p)
  eval (ClearText next) = do
    liftQuery $ query AcePlaceholder (action $ ChangeText "")
    pure next

  -- | Peek allows us to observe inputs going to the child components, here
  -- | we're using it to observe when the text is changed or copied inside the
  -- | ace component, and igoring all other inputs.
  peek :: Peek State Input (QueryF State AceState AceInput (Aff (AceEffects eff)) AcePlaceholder p) (ChildF AcePlaceholder AceInput)
  peek (ChildF AcePlaceholder q) = case q of
    ChangeText text _ -> modify _ { text = text }
    CopiedText text _ -> modify _ { copied = Just text }
    _ -> pure unit

-- | The main UI component - the parent component with installed ace component.
ui :: forall p eff. InstalledComponentP State AceState Input AceInput (Aff (AceEffects eff)) (ChildF AcePlaceholder AceInput) (Const Void) AcePlaceholder p
ui = install' container \AcePlaceholder -> Tuple (ace "test") initAceState

-- | Run the Halogen app.
main :: Eff (HalogenEffects (ace :: ACE, console :: CONSOLE)) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
