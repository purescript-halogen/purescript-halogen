module Button where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import HOC (class CanSet)

type State = Boolean

data Query a
  = Toggle a
  | Set Boolean a
  | IsOn (Boolean -> a)

instance canSetQuery :: CanSet Query where
  set = Set

data Message = Toggled Boolean

myButton :: forall m. H.Component HH.HTML Query Boolean Message m
myButton =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    Set value next -> do
      H.put value
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)
