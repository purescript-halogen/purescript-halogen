module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

data Query a = ToggleState a

ui :: forall m. H.Component HH.HTML Query Void m
ui = H.component { initialState, render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Toggle Button" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text (if state.on then "On" else "Off") ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    pure next

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui body
