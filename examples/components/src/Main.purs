module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..), maybe)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import Ticker (TickState, TickQuery(..), ticker)

data Query a = ReadTicks a

type State = { tickA :: Maybe Int, tickB :: Maybe Int }

initialState :: State
initialState = { tickA: Nothing, tickB: Nothing }

newtype TickSlot = TickSlot String

derive instance eqTickSlot :: Eq TickSlot
derive instance ordTickSlot :: Ord TickSlot

type State' g = H.ParentState State TickState Query TickQuery g TickSlot
type Query' = Coproduct Query (H.ChildF TickSlot TickQuery)

ui :: forall g. Functor g => H.Component (State' g) Query' g
ui = H.parentComponent { render, eval, peek: Nothing }
  where

  render :: State -> H.ParentHTML TickState Query TickQuery g TickSlot
  render st =
    HH.div_
      [ HH.slot (TickSlot "A") \_ -> { component: ticker, initialState: 100 }
      , HH.slot (TickSlot "B") \_ -> { component: ticker, initialState: 0 }
      , HH.p_
          [ HH.p_
              [ HH.text $ "Last tick readings - A: " <> (maybe "No reading" show st.tickA)
                                         <> ", B: " <> (maybe "No reading" show st.tickB)
              ]
          , HH.button
              [ HE.onClick (HE.input_ ReadTicks) ]
              [ HH.text "Update reading" ]
          ]
      ]

  eval :: Query ~> H.ParentDSL State TickState Query TickQuery g TickSlot
  eval (ReadTicks next) = do
    a <- H.query (TickSlot "A") (H.request GetTick)
    b <- H.query (TickSlot "B") (H.request GetTick)
    H.modify (\_ -> { tickA: a, tickB: b })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState initialState) body
