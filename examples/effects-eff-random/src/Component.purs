module Component (State, Query(..), ui) where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM, random)
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Maybe Number

data Query a = Regenerate a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (random :: RANDOM | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Nothing

  render :: State -> H.ComponentHTML Query
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

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff))
  eval = case _ of
    Regenerate next -> do
      newNumber <- H.liftEff random
      H.put (Just newNumber)
      pure next
