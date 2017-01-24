module RouteLog where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = { history :: Array String }

data Query a = ChangeRoute String a

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { history: [] }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Change the URL hash or choose an anchor link..." ]
      , HH.ul_
          [ HH.li_ [ HH.a [ HP.href "#link-a" ] [ HH.text "Link A" ] ]
          , HH.li_ [ HH.a [ HP.href "#link-b" ] [ HH.text "Link B" ] ]
          , HH.li_ [ HH.a [ HP.href "#link-c" ] [ HH.text "Link C" ] ]
          ]
      , HH.p_ [ HH.text "...to see it logged below:" ]
      , HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.history
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ChangeRoute msg next -> do
      H.modify \st -> { history: st.history `A.snoc` msg }
      pure next
