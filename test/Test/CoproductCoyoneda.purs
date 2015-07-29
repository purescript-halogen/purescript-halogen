-- | An example of using a query algebra `Coproduct f (Coyoneda g)`.
-- |
-- | The `inputCF` helper is used to lift a `g Unit` value into
-- | `EventHandler (Free (Coproduct f (Coyoneda g)) Unit)`.
-- |
-- | The `actionF` helper is used to lift a `f a` into
-- | `Free (Coproduct f (Coyoneda g)) a` for the driver.
module Test.CoproductCoyoneda where

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

instance functorInput :: Functor Input1 where
  map f (Tick a) = Tick (f a)

data Input2 a = UnTick a

type Input = Coproduct Input1 (Coyoneda Input2)

ui :: forall g p. (Functor g) => ComponentF State Input g p
ui = componentF render eval
  where

  render :: RenderF State p Input
  render (State n) =
    H.div_ [ H.h1 [ P.id_ "header" ] [ H.text "counter" ]
           , H.p_ [ H.text (show n) ]
           , H.button [ E.onClick (E.inputCF_ UnTick) ]
                      [ H.text "-1" ]
           ]

  eval :: Eval Input State g
  eval = coproduct eval1 (liftCoyonedaTF eval2)

  eval1 :: Eval Input1 State g
  eval1 (Tick next) = modify (\(State n) -> State (n + 1)) $> next

  eval2 :: Eval Input2 State g
  eval2 (UnTick next) = modify (\(State n) -> State (n - 1)) $> next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  { node: node, driver: driver } <- runUI ui initialState
  appendToBody node
  setInterval 1000 $ driver (actionF Tick)

setInterval :: forall e a. Int -> Aff e a -> Aff e Unit
setInterval ms a = later' ms $ do
  a
  setInterval ms a
