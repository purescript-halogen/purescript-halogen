module Example.Intro where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free (Free(), liftF, foldFree)

import Data.NaturalTransformation (Natural())

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

-- | The state of the application
type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

-- | Inputs to the state machine
data Input a = ToggleState a

data OutputF a = Log String a

type Output = Free OutputF

output :: String -> Output Unit
output msg = liftF (Log msg unit)

ui :: Component State Input Output
ui = component render eval
  where

  render :: Render State Input
  render state = H.div_
    [ H.h1_ [ H.text "Toggle Button" ]
    , H.button [ E.onClick (E.input_ ToggleState) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval Input State Input Output
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    liftH $ output "State was toggled"
    pure next

ui' :: forall eff. Component State Input (Aff (HalogenEffects (console :: CONSOLE | eff)))
ui' = interpret (foldFree evalOutput) ui
  where
  evalOutput :: Natural OutputF (Aff (HalogenEffects (console :: CONSOLE | eff)))
  evalOutput (Log msg next) = do
    Control.Monad.Aff.Console.log msg
    pure next

main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui' initialState
  appendToBody app.node
