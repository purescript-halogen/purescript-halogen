module Example.Effects.Eff.Random.Component where

import Prelude
import Effect.Aff (Aff)
import Effect.Random (random)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Maybe Number

data Query a = Regenerate a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = Nothing

  render :: forall m. State -> H.ComponentHTML (Query Unit) () m
  render state =
    let
      value = maybe "No number generated yet" show state
    in
      HH.div_ $
        [ HH.h1_ [ HH.text "Random number" ]
        , HH.p_ [ HH.text ("Current value: " <> value) ]
        , HH.button
            [ HE.onClick (HE.input_ Regenerate) ]
            [ HH.text "Generate new number" ]
        ]

  eval :: Query ~> H.HalogenM State (Query Unit) () Void Aff
  eval = case _ of
    Regenerate next -> do
      newNumber <- H.liftEffect random
      H.put (Just newNumber)
      pure next
