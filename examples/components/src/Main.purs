module Example.Components.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const())
import Data.NaturalTransformation (Natural())
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E

import Example.Components.Ticker (TickState(..), TickInput(..), ticker)

data Input a = ReadTicks a

type State = { tickA :: Maybe Int, tickB :: Maybe Int }

initialState :: State
initialState = { tickA: Nothing, tickB: Nothing }

newtype TickSlot = TickSlot String

instance eqTickSlot :: Eq TickSlot where
  eq (TickSlot x) (TickSlot y) = eq x y

instance ordTickSlot :: Ord TickSlot where
  compare (TickSlot x) (TickSlot y) = compare x y

uiContainer :: forall g p. (Functor g) => ParentComponent State TickState Input TickInput g TickSlot p
uiContainer = component render eval
  where

  render :: Render State Input TickSlot
  render st = H.div_ [ H.Slot (TickSlot "A")
                     , H.Slot (TickSlot "B")
                     , H.p_ [ H.p_ [ H.text $ "Last tick readings - A: " ++ (maybe "No reading" show st.tickA) ++ ", B: " ++ (maybe "No reading" show st.tickB) ]
                            , H.button [ E.onClick (E.input_ ReadTicks) ]
                                       [ H.text "Update reading" ]
                            ]
                     ]

  eval :: Eval Input State Input (QueryF State TickState TickInput g TickSlot p)
  eval (ReadTicks next) = do
    a <- query (TickSlot "A") (request GetTick)
    b <- query (TickSlot "B") (request GetTick)
    modify (\_ -> { tickA: a, tickB: b })
    pure next

ui :: forall g p. (Monad g, Plus g) => InstalledComponent State TickState Input TickInput g TickSlot p
ui = install uiContainer go
  where
  go :: TickSlot -> ComponentState TickState TickInput g p
  go (TickSlot "A") = Tuple ticker (TickState 0)
  go (TickSlot "B") = Tuple ticker (TickState 100)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
