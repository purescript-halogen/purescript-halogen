module Example.Components.Inputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action
  = Increment
  | Decrement

type State = Int

type ChildSlots =
  ( display :: Display.Slot Int
  )

_display = SProxy :: SProxy "display"

component :: forall q i o m. H.Component HH.HTML q i o m
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
        [ HH.slot _display 1 Display.component state absurd
        , HH.slot _display 2 Display.component (state * 2) absurd
        , HH.slot _display 3 Display.component (state * 3) absurd
        , HH.slot _display 4 Display.component (state * 10) absurd
        , HH.slot _display 5 Display.component (state * state) absurd
        ]
    , HH.button
        [ HE.onClick (\_ -> Just Increment) ]
        [ HH.text "+1"]
    , HH.button
        [ HE.onClick (\_ -> Just Decrement) ]
        [ HH.text "-1"]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Increment ->
    H.modify_ (_ + 1)
  Decrement ->
    H.modify_ (_ - 1)
