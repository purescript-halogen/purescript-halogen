module Main where

import Prelude

import Ace.Types (ACE)
import AceComponent (AceEffects, AceOutput(..), AceQuery(..), AceSlot, aceComponent)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

-- | The application state, which in this case just stores the current text in
-- | the editor.
type State = { text :: String }

-- | The query algebra for the app.
data Query a
  = ClearText a
  | HandleAceUpdate String a

type ChildSlots =
  ( ace :: AceSlot Unit
  )

_ace = SProxy :: SProxy "ace"

-- | The main UI component definition.
ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AceEffects eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { text: "" }

  render :: State -> H.ComponentHTML Query ChildSlots (Aff (AceEffects eff))
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
          [ HH.slot _ace unit aceComponent unit handleAceOuput ]
      , HH.p_
          [ HH.text ("Current text: " <> text) ]
      ]

  eval :: Query ~> H.HalogenM State Query ChildSlots Void (Aff (AceEffects eff))
  eval (ClearText next) = do
    _ <- H.query _ace unit $ H.action (ChangeText "")
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
