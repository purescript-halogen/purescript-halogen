module Example.Components.Inputs.Display where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

type Slot p = forall q. H.Slot q Void p

type Input = Int

type State = Int

data Action = HandleInput Int

component :: forall q o m. H.Component q Input o m
component =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< HandleInput
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.text "My input value is:"
    , HH.strong_ [ HH.text (show state) ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput n -> do
    oldN <- H.get
    when (oldN /= n) $ H.put n
