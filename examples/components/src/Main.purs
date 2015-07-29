module Example.Components.Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free (Free(), FreeC())
import Control.Plus (Plus)

import Data.Coyoneda (Natural())
import Data.Functor.Coproduct (Coproduct())
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

import Halogen
import Halogen.Query.StateF (StateF(), modify)
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

uiContainer :: forall g p. (Functor g) => ComponentFC State Input (QueryF State TickState (FreeC TickInput) TickPlaceholder p g) TickPlaceholder
uiContainer = componentFC render eval
  where

  render :: RenderFC State TickPlaceholder Input
  render st = H.div_ [ H.Placeholder (TickPlaceholder "A")
                     , H.Placeholder (TickPlaceholder "B")
                     , H.p_ [ H.p_ [ H.text $ "Last tick readings - A: " ++ (maybe "No reading" show st.tickA) ++ ", B: " ++ (maybe "No reading" show st.tickB) ]
                            , H.button [ E.onClick (E.inputFC_ ReadTicks) ]
                                       [ H.text "Update reading" ]
                            ]
                     ]

  eval :: forall g. (Functor g) => Natural Input (Free (Coproduct (StateF State) (QueryF State TickState (FreeC TickInput) TickPlaceholder p g)))
  eval (ReadTicks next) = do
    a <- liftQuery $ query (TickPlaceholder "A") (requestFC GetTick)
    b <- liftQuery $ query (TickPlaceholder "B") (requestFC GetTick)
    modify (\_ -> { tickA: a, tickB: b })
    pure next

ui :: forall g p. (Monad g, Plus g) => Component (InstalledState State TickState (FreeC TickInput) TickPlaceholder p g) (Coproduct (FreeC Input) (ChildF TickPlaceholder (FreeC TickInput))) g p
ui = installAll uiContainer go
  where
  go :: TickPlaceholder -> ComponentState TickState (FreeC TickInput) g p
  go (TickPlaceholder "A") = Tuple (TickState 0) ticker
  go (TickPlaceholder "B") = Tuple (TickState 100) ticker

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui { parent: initialState, children: mempty }
  appendToBody app.node
