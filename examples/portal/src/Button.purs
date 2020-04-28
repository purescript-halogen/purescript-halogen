module Example.Portal.Button (component) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

type Input = HTMLElement

type State = { enabled :: Boolean, target :: HTMLElement }

data Action = Toggle

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState target = { enabled: false, target }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "On" else "Off"
  in
    HH.div
      [ ]
      [ HH.text "Position in tree"
      , HH.portal state.target do
          HH.button
            [ HP.title label
            , HE.onClick \_ -> Toggle
            ]
            [ HH.text label ]
      ]

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st { enabled = not st.enabled }
