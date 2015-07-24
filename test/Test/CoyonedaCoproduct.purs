-- | An example of using a query algebra `Coyoneda (Coproduct f g)`.
-- |
-- | The `inputFC` helper is used to lift a `g Unit` value into
-- | `EventHandler (Free (Coyoneda (Coproduct f g)) Unit)`.
-- |
-- | The `actionFC` helper is used to lift a `f a` into
-- | `Free (Coyoneda (Coproduct f g) a` for the driver.
module Test.CoyonedaCoproduct where

import Prelude

import Control.Apply ((*>))
import Control.Plus (empty)
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
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Properties as P

data State = State Int

initialState :: State
initialState = State 0

data Input1 a = Tick a
data Input2 a = UnTick a

type Input = Coproduct Input1 Input2

ui :: forall g p. (Functor g) => ComponentFC State Input g p
ui = componentFC render eval
  where

  render :: forall p. RenderFC State p Input
  render (State n) =
    H.div_ [ H.h1 [ P.id_ "header" ] [ H.text "counter" ]
           , H.p_ [ H.text (show n) ]
           , H.button [ E.onClick (E.inputFC_ UnTick) ]
                      [ H.text "-1" ]
           ]

  eval :: Eval Input State g
  eval = coproduct eval1 eval2

  eval1 :: Eval Input1 State g
  eval1 (Tick next) = modify (\(State n) -> State (n + 1)) $> next

  eval2 :: Eval Input2 State g
  eval2 (UnTick next) = modify (\(State n) -> State (n - 1)) $> next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  { node: node, driver: driver } <- runUI ui initialState
  appendToBody node
  setInterval 1000 $ driver (actionFC Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
