module Main where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Free (Free(), liftF, foldFree)

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

data Query a = ToggleState a

data OutputF a = Log String a
type Output = Free OutputF

output :: String -> Output Unit
output msg = liftF (Log msg unit)

ui :: Component State Query Output
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

  eval :: Natural Query (ComponentDSL State Query Output)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    liftH $ output "State was toggled"
    pure next

ui' :: forall eff. Component State Query (Aff (HalogenEffects (console :: CONSOLE | eff)))
ui' = interpret (foldFree evalOutput) ui
  where
  evalOutput :: Natural OutputF (Aff (HalogenEffects (console :: CONSOLE | eff)))
  evalOutput (Log msg next) = do
    log msg
    pure next

main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui' initialState body
