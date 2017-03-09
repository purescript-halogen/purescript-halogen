module Container where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Reflection (class Given, given)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Button as Button

import I18n (I18n(..), I18n')

data Query a
  = HandleMessage String a

type State =
  { message ∷ Maybe String
  }

component
  :: forall m
   . Given I18n
  => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { render
    , eval
    , initialState: const { message: Nothing }
    , receiver: const Nothing
    }
  where
  i18n :: I18n' String
  i18n = un I18n given

  render :: State -> H.ParentHTML Query Button.Query Unit m
  render st =
    HH.div_
      [ HH.p_ [ HH.text i18n.description ]
      , HH.slot unit Button.component unit (HE.input HandleMessage)
      , HH.p_ [ HH.text (fromMaybe "" st.message) ]
      ]

  eval :: Query ~> H.ParentDSL State Query Button.Query Unit Void m
  eval (HandleMessage message next) =
    H.modify _ { message = Just message } $> next
