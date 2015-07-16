module Test.Example.Counter where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Free (FreeC(), runFreeCM)
import Control.Monad.Rec.Class (MonadRec)
import Control.Monad.State.Class (modify)
import Control.Monad.State.Trans (StateT())

import Data.Coyoneda (Natural())
import Data.DOM.Simple.Window (globalWindow, setInterval)
import Data.Functor (($>))

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P

-- | The state of the application
data State = State Int

-- | The initial state
initialState :: State
initialState = State 0

-- | Inputs to the state machine
data Input a = Tick a

-- | The main UI component.
ui :: forall g p. (MonadRec g) => ComponentFC State Input g p
ui = componentFC render eval
  where

  render :: State -> H.HTML p (FreeC Input Unit)
  render (State n) =
    H.div_ [ H.h1 [ P.id_ "header" ] [ H.text "counter" ]
           , H.p_ [ H.text (show n) ]
           ]

  eval :: Natural Input (StateT State g)
  eval (Tick next) = modify (\(State n) -> State (n + 1)) $> next

-- | Run the app
main :: Eff (HalogenEffects ()) Unit
main = do
  ui <- runUI ui initialState
  appendToBody ui.node
  setInterval globalWindow 1000.0 $ ui.driver (actionFC Tick)
  pure unit
