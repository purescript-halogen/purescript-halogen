module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Free (FreeC())
import Control.Monad.Rec.Class (MonadRec)
import Control.Monad.State.Class (modify)
import Control.Monad.State.Trans (StateT())

import Data.Coyoneda (Natural())
import Data.Functor (($>))

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

-- | The state of the application
newtype State = State { on :: Boolean }

-- | The initial state
initialState = State { on: false }

-- | Inputs to the state machine
data Input a = ToggleState a

ui :: forall g p. (MonadRec g) => ComponentFC State Input g p
ui = componentFC render eval
  where

  render :: State -> H.HTML p (FreeC Input Unit)
  render (State s) =
    H.div_ [ H.h1_ [ H.text "Toggle Button" ]
           , H.button [ E.onClick (E.inputFC_ ToggleState) ]
                      [ H.text (if s.on then "On" else "Off") ]
           ]

  eval :: Natural Input (StateT State g)
  eval (ToggleState next) = modify (\(State s) -> State { on: not s.on }) $> next

-- | Run the app
main :: Eff (HalogenEffects ()) Unit
main = do
  ui <- runUI ui initialState
  appendToBody ui.node
