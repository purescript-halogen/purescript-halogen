module ComponentB where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Int

data Query a
  = Increment a
  | GetCount (Int -> a)

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 0

  render :: State -> H.ComponentHTML Query
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

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Increment next) = do
    H.modify (_ + 1)
    pure next
  eval (GetCount reply) = do
    reply <$> H.get
