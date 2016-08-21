module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)

import Data.Maybe (Maybe(..), maybe)
import Data.Lazy (defer)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import Ticker (TickQuery(..), ticker)

data Query a
  = ReadTicks a
  | Swap a

type State = { tickA :: Maybe Int, tickB :: Maybe Int, swapped :: Boolean }

initialState :: State
initialState = { tickA: Nothing, tickB: Nothing, swapped: false }

newtype TickSlot = TickSlot String
derive instance eqTickSlot :: Eq TickSlot
derive instance ordTickSlot :: Ord TickSlot

ui :: forall g. Applicative g => H.Component Query g
ui = H.parentComponent { render, eval, initialState }
  where
  render :: State -> H.ParentHTML Query TickQuery g TickSlot
  render { swapped, tickA, tickB } =
    HH.div_
      [ if swapped then HH.slot (TickSlot "B") (defer \_ -> ticker 0) else HH.slot (TickSlot "A") (defer \_ -> ticker 100)
      , if swapped then HH.slot (TickSlot "A") (defer \_ -> ticker 100) else HH.slot (TickSlot "B") (defer \_ -> ticker 0)
      , HH.p_
          [ HH.p_
              [ HH.text
                  $ "Last tick readings - "
                  <> "A: " <> (maybe "No reading" show tickA) <> ", "
                  <> "B: " <> (maybe "No reading" show tickB)
              ]
          , HH.button
              [ HE.onClick (HE.input_ ReadTicks) ]
              [ HH.text "Update reading" ]
          ]
      , HH.button
          [ HE.onClick (HE.input_ Swap) ]
          [ HH.text "Swap tickers" ]
      ]

  eval :: Query ~> Free (H.ParentDSL State Query TickQuery g TickSlot)
  eval (ReadTicks next) = do
    a <- H.query (TickSlot "A") (H.request GetTick)
    b <- H.query (TickSlot "B") (H.request GetTick)
    H.modify (\st -> st { tickA = a, tickB = b })
    pure next
  eval (Swap next) = do
    H.modify (\st -> st { swapped = not st.swapped })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui body
