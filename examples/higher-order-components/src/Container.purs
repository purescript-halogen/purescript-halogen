module Container where

import Prelude
import Data.Maybe (Maybe(..), maybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Button as Button
import HOC as HOC

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

type ChildQuery = HOC.Query Button.Query Boolean Button.Message

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { toggleCount: 0
    , buttonState: Nothing
    }

  render :: State -> H.ParentHTML Query ChildQuery Slot m
  render state =
    HH.div_
      [ HH.slot Button (HOC.factory Button.myButton) true (HE.input HandleButton)
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

  eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void m
  eval = case _ of
    HandleButton (Button.Toggled _) next -> do
      H.modify (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    CheckButtonState next -> do
      buttonState <- H.query Button $ HOC.liftQuery $ H.request Button.IsOn
      H.modify (_ { buttonState = buttonState })
      pure next
