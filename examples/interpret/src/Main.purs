module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free, liftF, foldFree)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

data Query a = ToggleState a

data OutputF a = Log String a
type Output = Free OutputF

output :: String -> Output Unit
output msg = liftF (Log msg unit)

ui :: H.Component State Query Output
ui = H.component { render, eval }
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

  eval :: Query ~> H.ComponentDSL State Query Output
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    H.liftH $ output "State was toggled"
    pure next

ui' :: forall eff. H.Component State Query (Aff (H.HalogenEffects (console :: CONSOLE | eff)))
ui' = H.interpret (foldFree evalOutput) ui
  where
  evalOutput :: OutputF ~> Aff (H.HalogenEffects (console :: CONSOLE | eff))
  evalOutput (Log msg next) = do
    log msg
    pure next

main :: Eff (H.HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui' initialState body
