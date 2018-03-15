module Display where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

type Input = Int

type State = Int

data Query a = HandleInput Int a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: id
    , lifecycle
    , render
    , eval
    }
  where

  lifecycle :: H.Lifecycle Input -> Maybe (Query Unit)
  lifecycle = case _ of
    H.Receive i -> Just (H.action (HandleInput i))
    _ -> Nothing

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.text "My input value is:"
      , HH.strong_ [ HH.text (show state) ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput n next -> do
      oldN <- H.get
      when (oldN /= n) $ H.put n
      pure next
