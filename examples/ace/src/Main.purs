module Example.Ace.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Ace.AceComponent (AceOutput(..), AceQuery(..), AceSlot, aceComponent)
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
ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
  initialState :: State
  initialState = { text: "" }

  render :: State -> H.ComponentHTML (Query Unit) ChildSlots Aff
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
  eval :: Query ~> H.HalogenM State (Query Unit) ChildSlots Void Aff
  eval (ClearText next) = do
    _ <- H.query _ace unit $ H.action (ChangeText "")
    pure next
  eval (HandleAceUpdate text next) = do
    H.modify_ (_ { text = text })
    pure next

  handleAceOuput :: AceOutput -> Maybe (Query Unit)
  handleAceOuput (TextChanged text) = Just $ H.action $ HandleAceUpdate text

-- | Run the app!
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
