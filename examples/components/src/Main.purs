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

newtype TickPlaceholder = TickPlaceholder String

instance eqTickPlaceholder :: Eq TickPlaceholder where
  eq (TickPlaceholder x) (TickPlaceholder y) = eq x y

instance ordTickPlaceholder :: Ord TickPlaceholder where
  compare (TickPlaceholder x) (TickPlaceholder y) = compare x y

uiContainer :: forall g p. (Functor g) => ParentComponent State TickState Input TickInput g (Const Void) TickPlaceholder p
uiContainer = component render eval
  where

  render :: Render State Input TickPlaceholder
  render st = H.div_ [ H.Placeholder (TickPlaceholder "A")
                     , H.Placeholder (TickPlaceholder "B")
                     , H.p_ [ H.p_ [ H.text $ "Last tick readings - A: " ++ (maybe "No reading" show st.tickA) ++ ", B: " ++ (maybe "No reading" show st.tickB) ]
                            , H.button [ E.onClick (E.input_ ReadTicks) ]
                                       [ H.text "Update reading" ]
                            ]
                     ]

  eval :: Eval Input State Input (QueryF State TickState TickInput g TickPlaceholder p)
  eval (ReadTicks next) = do
    a <- query (TickPlaceholder "A") (request GetTick)
    b <- query (TickPlaceholder "B") (request GetTick)
    modify (\_ -> { tickA: a, tickB: b })
    pure next

ui :: forall g p. (Monad g, Plus g) => InstalledComponent State TickState Input TickInput g (Const Void) TickPlaceholder p
ui = install uiContainer go
  where
  go :: TickPlaceholder -> ComponentState TickState TickInput g p
  go (TickPlaceholder "A") = Tuple ticker (TickState 0)
  go (TickPlaceholder "B") = Tuple ticker (TickState 100)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
