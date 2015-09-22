module Example.MultiComponent where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const())
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen
import Halogen.Component.Inject (InjectC(), inl, inr, (:>))
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

import Example.ComponentA
import Example.ComponentB
import Example.ComponentC

newtype State = State { a :: Maybe Boolean, b :: Maybe Boolean, c :: Maybe Boolean }

initialState :: State
initialState = State { a: Nothing, b: Nothing, c: Nothing }

data Input a = ReadStates a

type ChildState = Either StateA (Either StateB StateC)
type ChildInput = Coproduct InputA (Coproduct InputB InputC)
type ChildSlot = Either SlotA (Either SlotB SlotC)

injA :: InjectC StateA ChildState InputA ChildInput SlotA ChildSlot
injA = inl

injB :: InjectC StateB ChildState InputB ChildInput SlotB ChildSlot
injB = inr :> inl

injC :: InjectC StateC ChildState InputC ChildInput SlotC ChildSlot
injC = inr :> inr

parent :: forall g. (Functor g) => ParentComponent State ChildState Input ChildInput g ChildSlot Void
parent = component render eval
  where

  render :: Render State Input ChildSlot
  render (State state) = H.div_
    [ H.div_ [ H.slot' injA SlotA ]
    , H.div_ [ H.slot' injB SlotB ]
    , H.div_ [ H.slot' injC SlotC ]
    , H.div_ [ H.text $ "Current states: " ++ show state.a ++ " / " ++ show state.b ++ " / " ++ show state.c ]
    , H.button [ E.onClick (E.input_ ReadStates) ] [ H.text "Read states" ]
    ]

  eval :: Eval Input State Input (QueryF State ChildState ChildInput g ChildSlot Void)
  eval (ReadStates next) = do
    a <- query' injA SlotA (request GetStateA)
    b <- query' injB SlotB (request GetStateB)
    c <- query' injC SlotC (request GetStateC)
    modify (const $ State { a: a, b: b, c: c })
    pure next

ui :: forall g. (Plus g) => InstalledComponent State ChildState Input ChildInput g ChildSlot Void
ui = install parent (either installA (either installB installC))
  where
  installA SlotA = createChild injA componentA initStateA
  installB SlotB = createChild injB componentB initStateB
  installC SlotC = createChild injC componentC initStateC

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
