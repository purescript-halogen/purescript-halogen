module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..), maybe)
import Data.Lazy (defer)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import Ticker (TickQuery(..), ticker)

data Query a = ReadTicks a

type State = { tickA :: Maybe Int, tickB :: Maybe Int, count :: Int }

initialState :: State
initialState = { tickA: Nothing, tickB: Nothing, count: 0 }

newtype TickSlot = TickSlot String
derive instance eqTickSlot :: Eq TickSlot
derive instance ordTickSlot :: Ord TickSlot

ui :: forall m. Applicative m => H.Component HH.HTML Query Void m
ui = H.parentComponent { render, eval, initialState }
  where
  render :: State -> H.ParentHTML Query TickQuery TickSlot m
  render { tickA, tickB, count } =
    HH.div_
      [ HH.slot (TickSlot "A") (defer \_ -> ticker 100) absurd
      , HH.slot (TickSlot "B") (defer \_ -> ticker 0) absurd
      , HH.p_
          [ HH.p_
              [ HH.text
                  $ "Last tick readings - "
                  <> "A: " <> (maybe "No reading" show tickA) <> ", "
                  <> "B: " <> (maybe "No reading" show tickB)
              ]
          , HH.button
              [ HE.onClick (HE.input_ ReadTicks) ]
              [ HH.text $ "Update reading (" <> show count <> ")" ]
          ]
      ]

  eval :: Query ~> H.ParentDSL State Query TickQuery TickSlot Void m
  eval (ReadTicks next) = do
    a <- H.query (TickSlot "A") (H.request GetTick)
    b <- H.query (TickSlot "B") (H.request GetTick)
    H.modify \st -> { tickA: a, tickB: b, count: st.count + 1 }
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
