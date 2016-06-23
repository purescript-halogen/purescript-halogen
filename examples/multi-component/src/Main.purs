module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import ComponentA (StateA, QueryA(..), SlotA(..), initStateA, componentA)
import ComponentB (StateB, QueryB(..), SlotB(..), initStateB, componentB)
import ComponentC (StateC, QueryC(..), SlotC(..), initStateC, componentC)

type State = { a :: Maybe Boolean, b :: Maybe Boolean, c :: Maybe Boolean }

initialState :: State
initialState = { a: Nothing, b: Nothing, c: Nothing }

data Query a = ReadStates a

type ChildState = Either StateA (Either StateB StateC)
type ChildQuery = Coproduct QueryA (Coproduct QueryB QueryC)
type ChildSlot = Either SlotA (Either SlotB SlotC)

cpA :: ChildPath StateA ChildState QueryA ChildQuery SlotA ChildSlot
cpA = cpL

cpB :: ChildPath StateB ChildState QueryB ChildQuery SlotB ChildSlot
cpB = cpR :> cpL

cpC :: ChildPath StateC ChildState QueryC ChildQuery SlotC ChildSlot
cpC = cpR :> cpR

type State' g = H.ParentState State ChildState Query ChildQuery g ChildSlot
type Query' = Coproduct Query (H.ChildF ChildSlot ChildQuery)

ui :: forall g. Functor g => H.Component (State' g) Query' g
ui = H.parentComponent { render, eval, peek: Nothing }
  where

  render :: State -> H.ParentHTML ChildState Query ChildQuery g ChildSlot
  render state = HH.div_
    [ HH.div_ [ HH.slot' cpA SlotA \_ -> { component: componentA, initialState: initStateA } ]
    , HH.div_ [ HH.slot' cpB SlotB \_ -> { component: componentB, initialState: initStateB } ]
    , HH.div_ [ HH.slot' cpC SlotC \_ -> { component: componentC, initialState: initStateC } ]
    , HH.div_ [ HH.text $ "Current states: " <> show state.a <> " / " <> show state.b <> " / " <> show state.c ]
    , HH.button [ HE.onClick (HE.input_ ReadStates) ] [ HH.text "Read states" ]
    ]

  eval :: Query ~> H.ParentDSL State ChildState Query ChildQuery g ChildSlot
  eval (ReadStates next) = do
    a <- H.query' cpA SlotA (H.request GetStateA)
    b <- H.query' cpB SlotB (H.request GetStateB)
    c <- H.query' cpC SlotC (H.request GetStateC)
    H.set { a, b, c }
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState initialState) body
