module Log where

import Prelude
import Data.Array as A
import Halogen as H
import Halogen.HTML as HH

type State = { messages :: Array String }

data Query a = AddMessage String a

component :: forall m. H.Component HH.HTML Query Void m
component = H.component { initialState, render, eval }
  where

  initialState :: State
  initialState = { messages: [] }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (AddMessage msg next) = do
    H.modify \st -> { messages: st.messages `A.snoc` msg }
    pure next
