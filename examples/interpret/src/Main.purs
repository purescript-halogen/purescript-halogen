module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free, liftF, foldFree)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type State = { on :: Boolean }

data Query a = ToggleState a

data MyAlgebra a = Log String a
type MyMonad = Free MyAlgebra

output :: String -> MyMonad Unit
output msg = liftF (Log msg unit)

ui :: H.Component HH.HTML Query Unit Void MyMonad
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Toggle Button" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text (if state.on then "On" else "Off") ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void MyMonad
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    H.lift $ output "State was toggled"
    pure next

ui' :: forall eff. H.Component HH.HTML Query Unit Void (Aff (HA.HalogenEffects (console :: CONSOLE | eff)))
ui' = H.hoist (foldFree evalMyAlgebra) ui
  where
  evalMyAlgebra :: MyAlgebra ~> Aff (HA.HalogenEffects (console :: CONSOLE | eff))
  evalMyAlgebra (Log msg next) = do
    log msg
    pure next

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui' unit body
