module Example.TextNodes.Elem (component) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Example.TextNodes.Test (mkTest)

type State = { enabled :: Boolean }

data Action = Toggle

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { enabled: false }

handleAction ∷ forall o m. Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ \st -> st { enabled = not st.enabled }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    label = if state.enabled then "Refresh component (On)" else "Refresh component (Off)"
  in
    HH.div_
      ( [ HH.button
            [ HP.title label
            , HE.onClick \_ -> Toggle
            ]
            [ HH.text label ]

        , HH.p_ [ HH.text "Elem" ]
        ] <> mkTest HH.div_
      )
