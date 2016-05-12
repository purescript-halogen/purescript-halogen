module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

data Query a = ToggleState a

ui :: forall g. (Functor g) => Component State Query g
ui = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
