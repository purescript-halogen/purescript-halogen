module Example.Components.List where

import Prelude

import Data.Array (snoc, init)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..))

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Example.Components.Ticker

data ListQuery a
  = AddTicker a
  | RemoveTicker a

type ListState = { nextId :: Int, tickerIds :: Array Int }

initialList :: ListState
initialList = { nextId: 1, tickerIds: [0] }

newtype TickSlot = TickSlot Int
derive instance genericTickSlot :: Generic TickSlot
instance eqTickSlot :: Eq TickSlot where eq = gEq
instance ordTickSlot :: Ord TickSlot where compare = gCompare

type ListQueryP = Coproduct ListQuery (ChildF TickSlot TickQuery)
type ListStateP g = InstalledState ListState TickState ListQuery TickQuery g TickSlot

listComponent :: forall g. (Functor g) => Component (ListStateP g) ListQueryP g
listComponent = parentComponent render eval
  where

  render :: ListState -> ParentHTML TickState ListQuery TickQuery g TickSlot
  render state =
    H.div_
      [ H.button
          [ E.onClick (E.input_ AddTicker) ]
          [ H.text "Add ticker "]
      , H.button
          [ E.onClick (E.input_ RemoveTicker) ]
          [ H.text "Remove ticker "]
      , H.ul_
          (map renderTicker state.tickerIds)
      ]

  renderTicker :: Int -> ParentHTML TickState ListQuery TickQuery g TickSlot
  renderTicker tickId =
    H.li_
      [ H.slot (TickSlot tickId) \_ ->
          { component: tickerComponent, initialState: TickState 0 }
      ]

  eval :: Natural ListQuery (ParentDSL ListState TickState ListQuery TickQuery g TickSlot)
  eval (AddTicker next) = do
    modify addTicker
    pure next
  eval (RemoveTicker next) = do
    modify removeTicker
    pure next

addTicker :: ListState -> ListState
addTicker st = { nextId: st.nextId + 1, tickerIds: st.tickerIds `snoc` st.nextId }

removeTicker :: ListState -> ListState
removeTicker st = case init st.tickerIds of
  Just tickerIds -> st { tickerIds = tickerIds }
  Nothing ->  st { tickerIds = [] }
