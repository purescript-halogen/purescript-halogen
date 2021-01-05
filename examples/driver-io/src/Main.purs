module Example.Driver.IO.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Example.Driver.IO.Button as B
import FRP.Event as Event
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI B.component unit body

  _ <- liftEffect $ Event.subscribe io.messages \(B.Toggled newState) -> do
    liftEffect $ log $ "Button was internally toggled to: " <> show newState
    pure Nothing

  state0 <- io.query $ H.mkRequest B.IsOn
  liftEffect $ log $ "The button state is currently: " <> show state0

  void $ io.query $ H.mkTell (B.SetState true)

  state1 <- io.query $ H.mkRequest B.IsOn
  liftEffect $ log $ "The button state is now: " <> show state1
