module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Lazy (defer)

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.VirtualDOM.Driver (runUI)

import ComponentA (QueryA(..), SlotA(..), componentA)
import ComponentB (QueryB(..), SlotB(..), componentB)
import ComponentC (QueryC(..), SlotC(..), componentC)

type State = { a :: Maybe Boolean, b :: Maybe Boolean, c :: Maybe Boolean }

initialState :: State
initialState = { a: Nothing, b: Nothing, c: Nothing }

data Query a = ReadStates a

type ChildQuery = QueryA <\/> QueryB <\/> QueryC <\/> Const Void
type ChildSlot = SlotA \/ SlotB \/ SlotC \/ Void

ui :: forall m. Applicative m => H.Component HH.HTML Query Void m
ui = H.parentComponent { render, eval, initialState }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = HH.div_
    [ HH.div_ [ HH.slot' CP.cp1 SlotA (defer \_ -> componentA) absurd ]
    , HH.div_ [ HH.slot' CP.cp2 SlotB (defer \_ -> componentB) absurd ]
    , HH.div_ [ HH.slot' CP.cp3 SlotC (defer \_ -> componentC) absurd ]
    , HH.div_ [ HH.text $ "Current states: " <> show state.a <> " / " <> show state.b <> " / " <> show state.c ]
    , HH.button [ HE.onClick (HE.input_ ReadStates) ] [ HH.text "Read states" ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (ReadStates next) = do
    a <- H.query' CP.cp1 SlotA (H.request GetStateA)
    b <- H.query' CP.cp2 SlotB (H.request GetStateB)
    c <- H.query' CP.cp3 SlotC (H.request GetStateC)
    H.put { a, b, c }
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
