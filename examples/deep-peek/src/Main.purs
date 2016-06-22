module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Const (Const, getConst)
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Maybe (Maybe(..))
import Data.Void (Void, absurd)

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H

import List
import Ticker

type Query = Const Void

type State = { count :: Int }

initialState :: State
initialState = { count: 0 }

newtype ListSlot = ListSlot String
derive instance eqListSlot :: Eq ListSlot
derive instance ordListSlot :: Ord ListSlot

type QueryP = Coproduct Query (ChildF ListSlot ListQueryP)
type StateP g = ParentState State (ListStateP g) Query ListQueryP g ListSlot

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
ui = parentComponent { render, eval, peek: Just peek }
  where

  render :: State -> ParentHTML (ListStateP g) Query ListQueryP g ListSlot
  render st =
    H.div_
      [ H.h1_
          [ H.text "List A" ]
      , H.slot (ListSlot "A") \_ ->
          { component: listComponent, initialState: parentState initialList }
      , H.hr []
      , H.h1_
          [ H.text "List B" ]
      , H.slot (ListSlot "B") \_ ->
          { component: listComponent, initialState: parentState initialList }
      , H.hr []
      , H.p_
          [ H.text $ "Total incremented count: " <> show st.count ]
      ]

  eval :: Query ~> (ParentDSL State (ListStateP g) Query ListQueryP g ListSlot)
  eval = absurd <<< getConst

  peek :: forall a. ChildF ListSlot ListQueryP a -> ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peek (ChildF _ q) = coproduct peekList peekTicker q

  peekList :: forall a. ListQuery a -> ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peekList _ =
    -- we're not actually interested in peeking on the list.
    -- instead of defining a function like this, an alternative would be to use
    -- `(const (pure unit))` in place of `peekList` in the `coproduct` function
    pure unit

  peekTicker :: forall a. ChildF TickSlot TickQuery a -> ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peekTicker (ChildF _ (Tick _)) = modify (\st -> { count: st.count + 1 })
  peekTicker _ = pure unit

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui (parentState initialState) body
