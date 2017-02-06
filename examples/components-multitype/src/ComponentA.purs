module ComponentA where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Boolean

data Query a
  = ToggleState a
  | GetState (Boolean -> a)

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Toggle me!" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text (if state then "On" else "Off") ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (ToggleState next) = do
    H.modify not
    pure next
  eval (GetState reply) = do
    reply <$> H.get
