module Example.MultiComponent where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const())
import Data.Either (Either(..))
import Data.Either.Nested (Either3(), either1of3, either2of3, either3of3, either3)
import Data.Functor (($>))
import Data.Functor.Coproduct.Nested (Coproduct3(), coproduct1of3, coproduct2of3, coproduct3of3)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen
import Halogen.Component.Inject (inl, inr, (:>))
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

type ChildState = Either3 StateA StateB StateC
type ChildInput = Coproduct3 InputA InputB InputC
type ChildPlaceholder = Either3 PlaceholderA PlaceholderB PlaceholderC

injA = inl :> inl
injB = inl :> inr
injC = inr

parent :: forall g p. (Functor g) => ParentComponent State ChildState Input ChildInput g (Const Void) ChildPlaceholder p
parent = component render eval
  where

  render :: Render State Input ChildPlaceholder
  render (State state) = H.div_
    [ H.div_ [ H.placeholder' injA PlaceholderA ]
    , H.div_ [ H.placeholder' injB PlaceholderB ]
    , H.div_ [ H.placeholder' injC PlaceholderC ]
    , H.div_ [ H.text $ "Current states: " ++ show state.a ++ " / " ++ show state.b ++ " / " ++ show state.c ]
    , H.button [ E.onClick (E.input_ ReadStates) ] [ H.text "Read states" ]
    ]

  eval :: Eval Input State Input (QueryF State ChildState ChildInput g ChildPlaceholder p)
  eval (ReadStates next) = do
    a <- query' injA PlaceholderA (request GetStateA)
    b <- query' injB PlaceholderB (request GetStateB)
    c <- query' injC PlaceholderC (request GetStateC)
    modify (const $ State { a: a, b: b, c: c })
    pure next

ui :: forall g p. (Monad g, Plus g) => InstalledComponent State ChildState Input ChildInput g (Const Void) ChildPlaceholder p
ui = install parent (either3 installA installB installC)
  where
  installA PlaceholderA = createChild injA componentA initStateA
  installB PlaceholderB = createChild injB componentB initStateB
  installC PlaceholderC = createChild injC componentC initStateC

main = launchAff $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
