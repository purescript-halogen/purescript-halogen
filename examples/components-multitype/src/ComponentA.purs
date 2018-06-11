module Example.Components.Multitype.ComponentA where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Boolean

data Query a
  = ToggleState a
  | GetState (Boolean -> a)

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
  initialState = false

  render :: State -> H.ComponentHTML Query () m
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Toggle me!" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text (if state then "On" else "Off") ]
      ]

  eval :: Query ~> H.HalogenM State Query () Void m
  eval (ToggleState next) = do
    H.modify_ not
    pure next
  eval (GetState reply) = do
    reply <$> H.get
