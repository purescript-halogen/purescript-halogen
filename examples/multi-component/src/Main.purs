module MultiComponent where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Plus (Plus)

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as H
import Halogen.Util (runHalogenAff, awaitBody)

import ComponentA
import ComponentB
import ComponentC

newtype State = State { a :: Maybe Boolean, b :: Maybe Boolean, c :: Maybe Boolean }

initialState :: State
initialState = State { a: Nothing, b: Nothing, c: Nothing }

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

type StateP g = ParentState State ChildState Query ChildQuery g ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent { render, eval, peek: Nothing }
  where

  render :: State -> ParentHTML ChildState Query ChildQuery g ChildSlot
  render (State state) = H.div_
    [ H.div_ [ H.slot' cpA SlotA \_ -> { component: componentA, initialState: initStateA } ]
    , H.div_ [ H.slot' cpB SlotB \_ -> { component: componentB, initialState: initStateB } ]
    , H.div_ [ H.slot' cpC SlotC \_ -> { component: componentC, initialState: initStateC } ]
    , H.div_ [ H.text $ "Current states: " ++ show state.a ++ " / " ++ show state.b ++ " / " ++ show state.c ]
    , H.button [ E.onClick (E.input_ ReadStates) ] [ H.text "Read states" ]
    ]

  eval :: Natural Query (ParentDSL State ChildState Query ChildQuery g ChildSlot)
  eval (ReadStates next) = do
    a <- query' cpA SlotA (request GetStateA)
    b <- query' cpB SlotB (request GetStateB)
    c <- query' cpC SlotC (request GetStateC)
    modify (const $ State { a: a, b: b, c: c })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui (parentState initialState) body
