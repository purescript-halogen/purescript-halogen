module Example.Components.Multitype.ComponentB where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Int

data Query a
  = Increment a
  | GetCount (Int -> a)

type Slot = H.Slot Query Void

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
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
  initialState = 0

  render :: State -> H.ComponentHTML Query () m
  render state =
    HH.div_
      [ HH.p_
          [ HH.text "Current value:"
          , HH.strong_ [ HH.text (show state) ]
          ]
      , HH.button
          [ HE.onClick (HE.input_ Increment) ]
          [ HH.text ("Increment") ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void m
  eval (Increment next) = do
    H.modify_ (_ + 1)
    pure next
  eval (GetCount reply) = do
    reply <$> H.get
