module Button where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Reflection (class Given, given)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import I18n (I18n(..), I18n')

data Query a
  = HandleClick a

component
  :: forall m
   . Given I18n
  => H.Component HH.HTML Query Unit String m
component =
  H.component
    { render
    , eval
    , initialState: id
    , receiver: const Nothing
    }
  where
  i18n :: I18n' String
  i18n = un I18n given

  render :: Unit -> H.ComponentHTML Query
  render _ =
    HH.button
      [ HE.onClick (HE.input_ HandleClick) ]
      [ HH.text i18n.buttonLabel ]

  eval :: Query ~> H.ComponentDSL Unit Query String m
  eval (HandleClick next) =
    H.raise i18n.congratulations $> next
