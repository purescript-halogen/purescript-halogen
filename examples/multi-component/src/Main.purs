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

import ComponentA as A
import ComponentB as B
import ComponentC as C

type State = { a :: Maybe Boolean, b :: Maybe Boolean, c :: Maybe Boolean }

initialState :: State
initialState = { a: Nothing, b: Nothing, c: Nothing }

data Query a = ReadStates a

type ChildState = Either A.State (Either B.State C.State)
type ChildQuery = Coproduct A.Query (Coproduct B.Query C.Query)
type ChildSlot = Either A.Slot (Either B.Slot C.Slot)

cpA :: ChildPath A.State ChildState A.Query ChildQuery A.Slot ChildSlot
cpA = cpL

cpB :: ChildPath B.State ChildState B.Query ChildQuery B.Slot ChildSlot
cpB = cpR :> cpL

cpC :: ChildPath C.State ChildState C.Query ChildQuery C.Slot ChildSlot
cpC = cpR :> cpR

type State' g = H.ParentState State ChildState Query ChildQuery g ChildSlot
type Query' = Coproduct Query (H.ChildF ChildSlot ChildQuery)

ui :: forall g. Functor g => H.Component (State' g) Query' g
ui = H.parentComponent { render, eval, peek: Nothing }
  where

  render :: State -> H.ParentHTML ChildState Query ChildQuery g ChildSlot
  render state = HH.div_
    [ HH.div_ [ HH.slot' cpA A.Slot \_ -> { component: A.component, initialState: A.initState } ]
    , HH.div_ [ HH.slot' cpB B.Slot \_ -> { component: B.component, initialState: B.initState } ]
    , HH.div_ [ HH.slot' cpC C.Slot \_ -> { component: C.component, initialState: C.initState } ]
    , HH.div_ [ HH.text $ "Current states: " <> show state.a <> " / " <> show state.b <> " / " <> show state.c ]
    , HH.button [ HE.onClick (HE.input_ ReadStates) ] [ HH.text "Read states" ]
    ]

  eval :: Query ~> H.ParentDSL State ChildState Query ChildQuery g ChildSlot
  eval (ReadStates next) = do
    a <- H.query' cpA A.Slot (H.request A.GetState)
    b <- H.query' cpB B.Slot (H.request B.GetState)
    c <- H.query' cpC C.Slot (H.request C.GetState)
    H.set { a, b, c }
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState initialState) body
