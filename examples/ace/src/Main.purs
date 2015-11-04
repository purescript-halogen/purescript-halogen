module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)

import Data.Generic (Generic, gEq, gCompare)
import Data.Functor.Coproduct (Coproduct())

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Ace.Types (ACE())
import AceComponent (AceState(), AceQuery(..), AceEffects(), ace, initAceState)

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

initialState :: State
initialState = { text: "" }

-- | The query algebra for the app.
data Query a = ClearText a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot

derive instance genericAceSlot :: Generic AceSlot
instance eqAceSlot :: Eq AceSlot where eq = gEq
instance ordAceSlot :: Ord AceSlot where compare = gCompare

-- | Synonyms for the "installed" version of the state and query algebra.
type StateP eff = InstalledState State AceState Query AceQuery (Aff (AceEffects eff)) AceSlot
type QueryP = Coproduct Query (ChildF AceSlot AceQuery)

-- | The main UI component definition.
ui :: forall eff. Component (StateP eff) QueryP (Aff (AceEffects eff))
ui = parentComponent' render eval peek
  where

  render :: State -> ParentHTML AceState Query AceQuery (Aff (AceEffects eff)) AceSlot
  render { text: text } =
    H.div_
      [ H.h1_
          [ H.text "ace editor" ]
      , H.div_
          [ H.p_
              [ H.button
                  [ E.onClick (E.input_ ClearText) ]
                  [ H.text "Clear" ]
              ]
          ]
      , H.div_
          [ H.slot AceSlot \_ -> { component: ace, initialState: initAceState } ]
      , H.p_
          [ H.text ("Current text: " ++ text) ]
      ]

  eval :: Natural Query (ParentDSL State AceState Query AceQuery (Aff (AceEffects eff)) AceSlot)
  eval (ClearText next) = do
    query AceSlot $ action (ChangeText "")
    pure next

  -- Peek allows us to observe inputs going to the child components, here
  -- we're using it to observe when the text is changed inside the ace
  -- component, and igoring any other inputs.
  peek :: forall a. ChildF AceSlot AceQuery a -> ParentDSL State AceState Query AceQuery (Aff (AceEffects eff)) AceSlot Unit
  peek (ChildF _ (ChangeText text _)) = modify _ { text = text }
  peek _ = pure unit

-- | Run the app!
main :: Eff (HalogenEffects (ace :: ACE, console :: CONSOLE)) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  onLoad $ appendToBody app.node
