module Example.Driver.IO.Main where

import Prelude

import Control.Coroutine as CR
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Example.Driver.IO.Button as B
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.component unit body

  io.subscribe $ CR.consumer \(B.Toggled newState) -> do
    liftEffect $ log $ "Button was internally toggled to: " <> show newState
    pure Nothing

  state0 ← io.query $ H.request B.IsOn
  liftEffect $ log $ "The button state is currently: " <> show state0

  void $ io.query $ H.tell (B.SetState true)

  state1 ← io.query $ H.request B.IsOn
  liftEffect $ log $ "The button state is now: " <> show state1
