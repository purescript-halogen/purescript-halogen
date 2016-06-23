module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import Ace.Types (ACE)
import AceComponent (AceState, AceQuery(..), AceEffects, ace, initAceState)

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

initialState :: State
initialState = { text: "" }

-- | The query algebra for the app.
data Query a = ClearText a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot

derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

-- | Synonyms for the "installed" version of the state and query algebra.
type StateP eff = H.ParentState State AceState Query AceQuery (Aff (AceEffects eff)) AceSlot
type QueryP = Coproduct Query (H.ChildF AceSlot AceQuery)

-- | The main UI component definition.
ui :: forall eff. H.Component (StateP eff) QueryP (Aff (AceEffects eff))
ui = H.parentComponent { render, eval, peek: Just peek }
  where

  render :: State -> H.ParentHTML AceState Query AceQuery (Aff (AceEffects eff)) AceSlot
  render { text: text } =
    HH.div_
      [ HH.h1_
          [ HH.text "ace editor" ]
      , HH.div_
          [ HH.p_
              [ HH.button
                  [ HE.onClick (HE.input_ ClearText) ]
                  [ HH.text "Clear" ]
              ]
          ]
      , HH.div_
          [ HH.slot AceSlot \_ -> { component: ace, initialState: initAceState } ]
      , HH.p_
          [ HH.text ("Current text: " <> text) ]
      ]

  eval :: Query ~> H.ParentDSL State AceState Query AceQuery (Aff (AceEffects eff)) AceSlot
  eval (ClearText next) = do
    H.query AceSlot $ H.action (ChangeText "")
    pure next

  -- Peek allows us to observe inputs going to the child components, here
  -- we're using it to observe when the text is changed inside the ace
  -- component, and igoring any other inputs.
  peek :: forall a. H.ChildF AceSlot AceQuery a -> H.ParentDSL State AceState Query AceQuery (Aff (AceEffects eff)) AceSlot Unit
  peek (H.ChildF _ (ChangeText text _)) = H.modify _ { text = text }
  peek _ = pure unit

-- | Run the app!
main :: Eff (H.HalogenEffects (ace :: ACE, console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState initialState) body
