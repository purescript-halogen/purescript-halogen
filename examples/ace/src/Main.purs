module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

import Ace.Types (ACE)
import AceComponent (AceEffects, AceOutput(..), AceQuery(..), aceComponent)

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

-- | The query algebra for the app.
data Query a
  = ClearText a
  | HandleAceUpdate String a

-- | The slot address type for the Ace component.
data AceSlot = AceSlot
derive instance eqAceSlot :: Eq AceSlot
derive instance ordAceSlot :: Ord AceSlot

-- | The main UI component definition.
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AceEffects eff))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { text: "" }

  render :: State -> H.ParentHTML Query AceQuery AceSlot (Aff (AceEffects eff))
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
          [ HH.slot AceSlot aceComponent unit handleAceOuput ]
      , HH.p_
          [ HH.text ("Current text: " <> text) ]
      ]

  eval :: Query ~> H.ParentDSL State Query AceQuery AceSlot Void (Aff (AceEffects eff))
  eval (ClearText next) = do
    H.query AceSlot $ H.action (ChangeText "")
    pure next
  eval (HandleAceUpdate text next) = do
    H.modify (_ { text = text })
    pure next

  handleAceOuput :: AceOutput -> Maybe (Query Unit)
  handleAceOuput (TextChanged text) = Just $ H.action $ HandleAceUpdate text

-- | Run the app!
main :: Eff (HA.HalogenEffects (ace :: ACE, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
