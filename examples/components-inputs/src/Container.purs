module Example.Components.Inputs.Container where

import Prelude

import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

data Action
  = Increment
  | Decrement

type State = Int

type ChildSlots =
  ( display :: Display.Slot Int
  )

_display = Proxy :: Proxy "display"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = 1

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.ul_
        [ HH.slot_ _display 1 Display.component state
        , HH.slot_ _display 2 Display.component (state * 2)
        , HH.slot_ _display 3 Display.component (state * 3)
        , HH.slot_ _display 4 Display.component (state * 10)
        , HH.slot_ _display 5 Display.component (state * state)
        ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text "+1"]
    , HH.button
        [ HE.onClick \_ -> Decrement ]
        [ HH.text "-1"]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Increment ->
    H.modify_ (_ + 1)
  Decrement ->
    H.modify_ (_ - 1)
