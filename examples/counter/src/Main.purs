module Main where

import Prelude

import Control.Monad.Aff (Aff, later')
import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

type State = Int

initialState :: State
initialState = 0

data Query a = Tick a

ui :: forall m. H.Component HH.HTML Query Void m
ui = H.component { render, eval, initialState }
  where

  render :: State -> H.ComponentHTML Query
  render n =
    HH.div_
      [ HH.h1
          [ HP.id_ "header" ]
          [ HH.text "counter" ]
      , HH.p_
          [ HH.text (show n) ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (Tick next) = do
    H.modify (_ + 1)
    pure next

-- | Run the app
main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  app <- runUI ui body
  setInterval 1000 $ app.query (H.action Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
