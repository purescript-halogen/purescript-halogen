module Main where

import Prelude

import Control.Monad.Aff (Aff(), later')
import Control.Monad.Eff (Eff())

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

newtype State = State Int

initialState :: State
initialState = State 0

data Query a = Tick a

ui :: forall g. Component State Query g
ui = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render (State n) =
    H.div_
      [ H.h1
          [ P.id_ "header" ]
          [ H.text "counter" ]
      , H.p_
          [ H.text (show n) ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (Tick next) = do
    modify (\(State n) -> State (n + 1))
    pure next

-- | Run the app
main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI ui initialState body
  setInterval 1000 $ driver (action Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
