module Example.Components.Ticker where

import Prelude

import Data.Functor (($>))

import Halogen
import Halogen.Query.StateF (modify, get)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E

data TickState = TickState Int

data TickInput a
  = Tick a
  | GetTick (Int -> a)

ticker :: forall g p. (Functor g) => ComponentFC TickState TickInput g p
ticker = componentFC render eval
  where

  render :: RenderFC TickState p TickInput
  render (TickState n) =
    H.div_ [ H.h1 [ P.id_ "header" ] [ H.text "counter" ]
           , H.p_ [ H.text (show n) ]
           , H.button [ E.onClick (E.inputFC_ Tick) ] [ H.text "Tick" ]
           ]

  eval :: Eval TickInput TickState g
  eval (Tick next) = modify (\(TickState n) -> TickState (n + 1)) $> next
  eval (GetTick k) = do
    TickState n <- get
    pure (k n)
