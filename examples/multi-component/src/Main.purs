module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Either (Either)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))
import Data.Lazy (defer)

import Halogen as H
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
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

type ChildQuery = Coproduct QueryA (Coproduct QueryB QueryC)
type ChildSlot = Either SlotA (Either SlotB SlotC)

cpA :: ChildPath QueryA ChildQuery SlotA ChildSlot
cpA = cpL

cpB :: ChildPath QueryB ChildQuery SlotB ChildSlot
cpB = cpR :> cpL

cpC :: ChildPath QueryC ChildQuery SlotC ChildSlot
cpC = cpR :> cpR

ui :: forall m. Applicative m => H.Component HH.HTML Query Void m
ui = H.parentComponent { render, eval, initialState }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = HH.div_
    [ HH.div_ [ HH.slot' cpA SlotA (defer \_ -> componentA) absurd ]
    , HH.div_ [ HH.slot' cpB SlotB (defer \_ -> componentB) absurd ]
    , HH.div_ [ HH.slot' cpC SlotC (defer \_ -> componentC) absurd ]
    , HH.div_ [ HH.text $ "Current states: " <> show state.a <> " / " <> show state.b <> " / " <> show state.c ]
    , HH.button [ HE.onClick (HE.input_ ReadStates) ] [ HH.text "Read states" ]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (ReadStates next) = do
    a <- H.query' cpA SlotA (H.request GetStateA)
    b <- H.query' cpB SlotB (H.request GetStateB)
    c <- H.query' cpC SlotC (H.request GetStateC)
    H.put { a, b, c }
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
