module Example.HOC.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Example.HOC.HOC (class CanSet)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

data Query a
  = Toggle a
  | Set Boolean a
  | IsOn (Boolean -> a)

instance canSetQuery :: CanSet Query where
  set = Set

data Message = Toggled Boolean

type Slot = H.Slot Query Boolean Message

myButton :: forall m. H.Component HH.HTML Query Boolean Message m
myButton =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  render :: State -> H.ComponentHTML Query () m
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Query ~> H.HalogenM State Query () Message m
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
