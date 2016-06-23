module List where

import Prelude

import Data.Array (snoc, init)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH

import Ticker (TickQuery, TickState, tickerComponent)

data ListQuery a
  = AddTicker a
  | RemoveTicker a

type ListState = { nextId :: Int, tickerIds :: Array Int }

initialList :: ListState
initialList = { nextId: 1, tickerIds: [0] }

newtype TickSlot = TickSlot Int
derive instance eqTickSlot :: Eq TickSlot
derive instance ordTickSlot :: Ord TickSlot

type ListQueryP = Coproduct ListQuery (H.ChildF TickSlot TickQuery)
type ListStateP g = H.ParentState ListState TickState ListQuery TickQuery g TickSlot

listComponent :: forall g. Functor g => H.Component (ListStateP g) ListQueryP g
listComponent = H.parentComponent { render, eval, peek: Nothing }
  where

  render :: ListState -> H.ParentHTML TickState ListQuery TickQuery g TickSlot
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick (HE.input_ AddTicker) ]
          [ HH.text "Add ticker "]
      , HH.button
          [ HE.onClick (HE.input_ RemoveTicker) ]
          [ HH.text "Remove ticker "]
      , HH.ul_
          (map renderTicker state.tickerIds)
      ]

  renderTicker :: Int -> H.ParentHTML TickState ListQuery TickQuery g TickSlot
  renderTicker tickId =
    HH.li_
      [ HH.slot (TickSlot tickId) \_ ->
          { component: tickerComponent, initialState: 0 }
      ]

  eval :: ListQuery ~> H.ParentDSL ListState TickState ListQuery TickQuery g TickSlot
  eval (AddTicker next) = do
    H.modify addTicker
    pure next
  eval (RemoveTicker next) = do
    H.modify removeTicker
    pure next

addTicker :: ListState -> ListState
addTicker st = { nextId: st.nextId + 1, tickerIds: st.tickerIds `snoc` st.nextId }

removeTicker :: ListState -> ListState
removeTicker st = case init st.tickerIds of
  Just tickerIds -> st { tickerIds = tickerIds }
  Nothing ->  st { tickerIds = [] }
