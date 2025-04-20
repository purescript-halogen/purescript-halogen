module Example.TextNodes.Keyed (component) where

import Prelude

import Data.Array as Array
import Data.Tuple (Tuple(..))
import Example.TextNodes.Test (mkTest)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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

staticKeyedDiv_ :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
staticKeyedDiv_ = HK.div_ <<< Array.mapWithIndex (\i el -> Tuple (show i) el)

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
        ] <> mkTest staticKeyedDiv_
      )
