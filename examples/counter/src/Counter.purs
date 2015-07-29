module Example.Counter where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Rec.Class (MonadRec)
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free

import Data.Functor (($>))
import Data.Functor.Coproduct
import Data.Coyoneda

import Halogen
import Halogen.Query.StateF
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

  render :: RenderFC State p Input
  render (State n) =
    H.div_ [ H.h1 [ P.id_ "header" ] [ H.text "counter" ]
           , H.p_ [ H.text (show n) ]
           ]

  eval :: Eval Input State g
  eval (Tick next) = modify (\(State n) -> State (n + 1)) $> next

-- | Run the app
main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  { node: node, driver: driver } <- runUI ui initialState
  appendToBody node
  setInterval 1000 $ driver (actionFC Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
