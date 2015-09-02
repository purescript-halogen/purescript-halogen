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

parent :: forall g p. (Functor g) => ParentComponent State ChildState Input ChildInput g (Const Void) ChildPlaceholder p
parent = component render eval
  where

  render :: Render State Input ChildPlaceholder
  render (State state) = H.div_
    [ H.div_ [ H.placeholder $ either1of3 PlaceholderA ]
    , H.div_ [ H.placeholder $ either2of3 PlaceholderB ]
    , H.div_ [ H.placeholder $ either3of3 PlaceholderC ]
    , H.div_ [ H.text $ "Current states: " ++ show state.a ++ " / " ++ show state.b ++ " / " ++ show state.c ]
    , H.button [ E.onClick (E.input_ ReadStates) ] [ H.text "Read states" ]
    ]

  eval :: Eval Input State Input (QueryF State ChildState ChildInput g ChildPlaceholder p)
  eval (ReadStates next) = do
    a <- liftQuery $ query (either1of3 PlaceholderA) (coproduct1of3 $ request GetStateA)
    b <- liftQuery $ query (either2of3 PlaceholderB) (coproduct2of3 $ request GetStateB)
    c <- liftQuery $ query (either3of3 PlaceholderC) (coproduct3of3 $ request GetStateC)
    modify (const $ State { a: a, b: b, c: c })
    pure next

ui :: forall g p. (Monad g, Plus g) => InstalledComponent State ChildState Input ChildInput g (Const Void) ChildPlaceholder p
ui = install parent (either3 installA installB installC)
  where
  installA PlaceholderA = Tuple (transformL $ transformL componentA) (either1of3 initStateA)
  installB PlaceholderB = Tuple (transformL $ transformR componentB) (either2of3 initStateB)
  installC PlaceholderC = Tuple (transformR componentC) (either3of3 initStateC)

main = launchAff $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
