module Test.Guide.Button where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

initialState :: State
initialState = false

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

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

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval = case _ of
  Toggle next -> do
    state <- H.get
    let nextState = not state
    H.put nextState
    H.raise $ Toggled nextState
    pure next
  IsOn continue -> do
    state <- H.get
    pure (continue state)

myButton :: forall m. H.Component HH.HTML Query Message m
myButton = H.component { initialState, render, eval }
