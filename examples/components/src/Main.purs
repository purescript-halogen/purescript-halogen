module Example.Components.Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..), maybe)

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Example.Components.Ticker (TickState(..), TickInput(..), ticker)

data Input a = ReadTicks a

type State = { tickA :: Maybe Int, tickB :: Maybe Int }

initialState :: State
initialState = { tickA: Nothing, tickB: Nothing }

newtype TickSlot = TickSlot String

derive instance genericTickSlot :: Generic TickSlot
instance eqTickSlot :: Eq TickSlot where eq = gEq
instance ordTickSlot :: Ord TickSlot where compare = gCompare

uiContainer :: forall g p. (Functor g) => ParentComponent State TickState Input TickInput g TickSlot p
uiContainer = component render eval
  where

  render :: Render State Input TickSlot
  render st =
    H.div_ [ H.slot (TickSlot "A")
           , H.slot (TickSlot "B")
           , H.p_ [ H.p_ [ H.text $ "Last tick readings - A: " ++ (maybe "No reading" show st.tickA) ++ ", B: " ++ (maybe "No reading" show st.tickB) ]
                  , H.button [ E.onClick (E.input_ ReadTicks) ]
                             [ H.text "Update reading" ]
                  ]
           ]

  eval :: EvalP Input State TickState Input TickInput g TickSlot p
  eval (ReadTicks next) = do
    a <- query (TickSlot "A") (request GetTick)
    b <- query (TickSlot "B") (request GetTick)
    modify (\_ -> { tickA: a, tickB: b })
    pure next

ui :: forall g p. (Plus g) => InstalledComponent State TickState Input TickInput g TickSlot p
ui = install uiContainer mkTicker
  where
  mkTicker (TickSlot "A") = createChild ticker (TickState 100)
  mkTicker (TickSlot _) = createChild ticker (TickState 0)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
