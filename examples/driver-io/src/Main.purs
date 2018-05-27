module Main where

import Prelude

import Button as B
import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.myButton unit body

  io.subscribe $ CR.consumer \(B.Toggled newState) -> do
    liftEffect $ log $ "Button was toggled to: " <> show newState
    pure Nothing

  io.query $ H.action $ B.Toggle
  io.query $ H.action $ B.Toggle
  io.query $ H.action $ B.Toggle
