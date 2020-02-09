module Example.Effects.Effect.Random.Component where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import Effect.Random (random)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Action = Regenerate

type State = Maybe Number

component :: forall q i o m. MonadEffect m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    value = maybe "No number generated yet" show state
  in
    HH.div_ $
      [ HH.h1_ [ HH.text "Random number" ]
      , HH.p_ [ HH.text ("Current value: " <> value) ]
      , HH.button
          [ HE.onClick \_ -> Regenerate ]
          [ HH.text "Generate new number" ]
      ]

handleAction :: forall o m. MonadEffect m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Regenerate -> do
    newNumber <- H.liftEffect random
    H.put (Just newNumber)
