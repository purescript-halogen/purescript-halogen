module Main where

import Prelude
import Control.Coroutine as CR
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Button as B

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.myButton unit body

  io.subscribe $ CR.consumer \(B.Toggled newState) -> do
    log $ "Button was toggled to: " <> show newState
    pure Nothing

  io.query $ H.action $ B.Toggle
  io.query $ H.action $ B.Toggle
  io.query $ H.action $ B.Toggle
