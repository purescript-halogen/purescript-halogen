module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Const (Const, getConst)
import Data.Functor.Coproduct (Coproduct, coproduct)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)

import List (ListQueryP, ListStateP, TickSlot, ListQuery, initialList, listComponent)
import Ticker (TickQuery(..))

type Query = Const Void

type State = { count :: Int }

initialState :: State
initialState = { count: 0 }

newtype ListSlot = ListSlot String

derive instance eqListSlot :: Eq ListSlot
derive instance ordListSlot :: Ord ListSlot

type Query' = Coproduct Query (H.ChildF ListSlot ListQueryP)
type State' g = H.ParentState State (ListStateP g) Query ListQueryP g ListSlot

ui :: forall g. Functor g => H.Component (State' g) Query' g
ui = H.parentComponent { render, eval, peek: Just peek }
  where

  render :: State -> H.ParentHTML (ListStateP g) Query ListQueryP g ListSlot
  render st =
    HH.div_
      [ HH.h1_
          [ HH.text "List A" ]
      , HH.slot (ListSlot "A") \_ ->
          { component: listComponent, initialState: H.parentState initialList }
      , HH.hr []
      , HH.h1_
          [ HH.text "List B" ]
      , HH.slot (ListSlot "B") \_ ->
          { component: listComponent, initialState: H.parentState initialList }
      , HH.hr []
      , HH.p_
          [ HH.text $ "Total incremented count: " <> show st.count ]
      ]

  eval :: Query ~> H.ParentDSL State (ListStateP g) Query ListQueryP g ListSlot
  eval = absurd <<< getConst

  peek :: forall a. H.ChildF ListSlot ListQueryP a -> H.ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peek = coproduct peekList peekTicker <<< H.runChildF

  peekList :: forall a. ListQuery a -> H.ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peekList _ =
    -- we're not actually interested in peeking on the list.
    -- instead of defining a function like this, an alternative would be to use
    -- `(const (pure unit))` in place of `peekList` in the `coproduct` function
    pure unit

  peekTicker :: forall a. H.ChildF TickSlot TickQuery a -> H.ParentDSL State (ListStateP g) Query ListQueryP g ListSlot Unit
  peekTicker (H.ChildF _ (Tick _)) = H.modify (\st -> { count: st.count + 1 })
  peekTicker _ = pure unit

main :: Eff (H.HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState initialState) body
