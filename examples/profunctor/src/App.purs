module Example.Profunctor.App (Query, app) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.Profunctor.Percentage (Slot) as PercentageInput
import Example.Profunctor.Percentage (Percentage(..), percentageInput)
import Halogen as H
import Halogen.HTML as HH

type ChildSlots = ( percentageInput :: PercentageInput.Slot Unit )
_percentageInput :: SProxy "percentageInput"
_percentageInput = SProxy

type State = Percentage

data Query a = Query a

data Action = SetState State

app :: forall m. H.Component HH.HTML Query Unit Void m
app = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall i. i -> State
initialState _ = Percentage 0

render :: forall m. Percentage -> H.ComponentHTML Action ChildSlots m
render state = HH.div_
  [ HH.slot _percentageInput unit percentageInput state (Just <<< SetState)
  , HH.h1_ [ HH.text $ show state ]
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  SetState state ->
    H.put state
