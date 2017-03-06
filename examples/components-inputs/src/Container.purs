module Container where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Display as Display

data Query a
  = Increment a
  | Decrement a

type State = Int

newtype Slot = Slot Int

derive newtype instance eqSlot :: Eq Slot
derive newtype instance ordSlot :: Ord Slot

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = 1

  render :: State -> H.ParentHTML Query Display.Query Slot m
  render state =
    HH.div_
      [ HH.ul_
          [ HH.slot (Slot 1) Display.component state absurd
          , HH.slot (Slot 2) Display.component (state * 2) absurd
          , HH.slot (Slot 3) Display.component (state * 3) absurd
          , HH.slot (Slot 4) Display.component (state * 10) absurd
          , HH.slot (Slot 5) Display.component (state * state) absurd
          ]
      , HH.button
          [ HE.onClick (HE.input_ Increment) ]
          [ HH.text "+1"]
      , HH.button
          [ HE.onClick (HE.input_ Decrement) ]
          [ HH.text "-1"]
      ]

  eval :: Query ~> H.ParentDSL State Query Display.Query Slot Void m
  eval = case _ of
    Increment next -> do
      H.modify (_ + 1)
      pure next
    Decrement next -> do
      H.modify (_ - 1)
      pure next
