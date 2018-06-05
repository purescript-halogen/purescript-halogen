module Main where

import Prelude

import Child as Child
import Data.Array (snoc, filter, reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type State =
  { currentId :: Int
  , slots :: Array Int
  }

initialState :: State
initialState =
  { currentId: 0
  , slots: []
  }

data Query a
  = Initialize a
  | Finalize a
  | Add a
  | Reverse a
  | Remove Int a
  | ReportRoot String a

ui :: H.Component HH.HTML Query Unit Void Aff
ui = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  render :: State -> H.ParentHTML Query Child.Query Int Aff
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick (HE.input_ Add) ]
          [ HH.text "Add" ]
      , HH.button
          [ HE.onClick (HE.input_ Reverse) ]
          [ HH.text "Reverse" ]
      , HK.ul_ $ flip map state.slots \sid ->
          Tuple (show sid) $
            HH.li_
              [ HH.button
                  [ HE.onClick (HE.input_ $ Remove sid) ]
                  [ HH.text "Remove" ]
              , HH.slot sid (Child.child sid) unit (listen sid)
              ]
      ]

  eval :: Query ~> H.ParentDSL State Query Child.Query Int Void Aff
  eval (Initialize next) = do
    H.liftEffect $ log "Initialize Root"
    pure next
  eval (Finalize next) = do
    pure next
  eval (Add next) = do
    H.modify_ \s ->
      { currentId: s.currentId + 1
      , slots: snoc s.slots s.currentId
      }
    pure next
  eval (Remove id next) = do
    H.modify_ \s -> s { slots = filter (not <<< eq id) s.slots }
    pure next
  eval (Reverse next) = do
    H.modify_ \s -> s { slots = reverse s.slots }
    pure next
  eval (ReportRoot msg next) = do
    H.liftEffect $ log ("Root >>> " <> msg)
    pure next

  listen :: Int -> Child.Message -> Maybe (Query Unit)
  listen i = Just <<< case _ of
    Child.Initialized -> H.action $ ReportRoot ("Heard Initialized from child" <> show i)
    Child.Finalized -> H.action $ ReportRoot ("Heard Finalized from child" <> show i)
    Child.Reported msg -> H.action $ ReportRoot ("Re-reporting from child" <> show i <> ": " <> msg)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
