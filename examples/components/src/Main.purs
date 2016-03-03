module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Plus (Plus)

import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..), maybe)

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

import Ticker (TickState(), TickQuery(..), ticker)

data Query a = ReadTicks a

type State = { tickA :: Maybe Int, tickB :: Maybe Int }

initialState :: State
initialState = { tickA: Nothing, tickB: Nothing }

newtype TickSlot = TickSlot String

derive instance genericTickSlot :: Generic TickSlot
instance eqTickSlot :: Eq TickSlot where eq = gEq
instance ordTickSlot :: Ord TickSlot where compare = gCompare

type StateP g = ParentState State TickState Query TickQuery g TickSlot
type QueryP = Coproduct Query (ChildF TickSlot TickQuery)

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent { render, eval, peek: Nothing }
  where

  render :: State -> ParentHTML TickState Query TickQuery g TickSlot
  render st =
    H.div_
      [ H.slot (TickSlot "A") \_ -> { component: ticker, initialState: 100 }
      , H.slot (TickSlot "B") \_ -> { component: ticker, initialState: 0 }
      , H.p_
          [ H.p_
              [ H.text $ "Last tick readings - A: " ++ (maybe "No reading" show st.tickA)
                                         ++ ", B: " ++ (maybe "No reading" show st.tickB)
              ]
          , H.button
              [ E.onClick (E.input_ ReadTicks) ]
              [ H.text "Update reading" ]
          ]
      ]

  eval :: Natural Query (ParentDSL State TickState Query TickQuery g TickSlot)
  eval (ReadTicks next) = do
    a <- query (TickSlot "A") (request GetTick)
    b <- query (TickSlot "B") (request GetTick)
    modify (\_ -> { tickA: a, tickB: b })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui (parentState initialState) body
