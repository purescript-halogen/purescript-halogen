module ComponentA where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE

newtype State = State { on :: Boolean }

initState :: State
initState = State { on: false }

data Query a
  = ToggleState a
  | GetState (Boolean -> a)

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: forall g. H.Component State Query g
component = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render (State state) = HH.div_
    [ HH.h1_ [ HH.text "Toggle Button A" ]
    , HH.button
        [ HE.onClick (HE.input_ ToggleState) ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

  eval :: Query ~> H.ComponentDSL State Query g
  eval (ToggleState next) = do
    H.modify (\(State state) -> State { on: not state.on })
    pure next
  eval (GetState continue) = do
    b <- H.gets (\(State state) -> state.on)
    pure (continue b)
