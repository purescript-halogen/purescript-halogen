module Display where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = Int

type State = Int

data Query a = HandleInput Int a

type Slot = H.Slot Query Void

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: HE.input HandleInput
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  render :: State -> H.ComponentHTML Query () m
  render state =
    HH.div_
      [ HH.text "My input value is:"
      , HH.strong_ [ HH.text (show state) ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      when (oldN /= n) $ H.put n
      pure next
