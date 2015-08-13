module Example.Intro where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Functor (($>))

import Halogen
import Halogen.Query.StateF (modify)
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

-- | The state of the application
newtype State = State { on :: Boolean }

initialState :: State
initialState = State { on: false }

-- | Inputs to the state machine
data Input a = ToggleState a

ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
  where

  render :: Render State Input p
  render (State state) = H.div_
    [ H.h1_ [ H.text "Toggle Button" ]
    , H.button [ E.onClick (E.input_ ToggleState) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval Input State Input g
  eval (ToggleState next) = modify (\(State state) -> State { on: not state.on }) $> next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  appendToBody app.node
