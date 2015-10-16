module Example.Intro where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

-- | The state of the application
type State = { on :: Boolean }

initialState :: State
initialState = { on: false }

-- | Querys to the state machine
data Query a = ToggleState a

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where

  render :: Render State Query
  render state = H.div_
    [ H.h1_ [ H.text "Toggle Button" ]
    , H.button [ E.onClick (E.input_ ToggleState) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval Query State Query g
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  appendToBody app.node
