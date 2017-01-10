module Container where

import Prelude
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Button as Button

data Query a
  = HandleButton Button.Message a
  | CheckButtonState a

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

data Slot = Button
derive instance eqTickSlot :: Eq Slot
derive instance ordTickSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Void m
component = H.parentComponent { render, eval, initialState }
  where

  initialState :: State
  initialState =
    { toggleCount: 0
    , buttonState: Nothing }

  render :: State -> H.ParentHTML Query Button.Query Slot m
  render state =
    HH.div_
      [ HH.slot Button (defer \_ -> Button.myButton) (HE.input HandleButton)
      , HH.p_
          [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
      , HH.p_
          [ HH.text
              $ "Last time I checked, the button was: "
              <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
              <> ". "
          , HH.button
              [ HE.onClick (HE.input_ CheckButtonState) ]
              [ HH.text "Check now" ]
          ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Slot Void m
  eval = case _ of
    HandleButton (Button.Toggled _) next -> do
      H.modify (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    CheckButtonState next -> do
      buttonState <- H.query Button $ H.request Button.IsOn
      H.modify (_ { buttonState = buttonState })
      pure next
