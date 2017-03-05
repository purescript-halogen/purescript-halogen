module Display where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Input = Int

type State = Int

data Query a = HandleInput Int a

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

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
