module Example.Lifecycle.Main where

import Prelude

import Data.Array (snoc, filter, reverse)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Example.Lifecycle.Child as Child
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))

type State =
  { currentId :: Int
  , slots :: Array Int
  }

initialState :: State
initialState =
  { currentId: 0
  , slots: []
  }

data Action
  = Initialize
  | Finalize
  | Add
  | Reverse
  | Remove Int
  | ReportRoot String

type ChildSlots =
  ( child :: H.Slot (Const Void) Child.Message Int
  )

_child = Proxy :: Proxy "child"

ui :: forall f. H.Component f Unit Void Aff
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval (H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        })
    }
  where
  render :: State -> H.ComponentHTML Action ChildSlots Aff
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick \_ -> Add ]
          [ HH.text "Add" ]
      , HH.button
          [ HE.onClick \_ -> Reverse ]
          [ HH.text "Reverse" ]
      , HK.ul_ $ flip map state.slots \sid ->
          Tuple (show sid) $
            HH.li_
              [ HH.button
                  [ HE.onClick \_ -> Remove sid ]
                  [ HH.text "Remove" ]
              , HH.slot _child sid (Child.child sid) unit (listen sid)
              ]
      ]

  handleAction :: forall o. Action -> H.HalogenM State Action ChildSlots o Aff Unit
  handleAction Initialize =
    H.liftEffect $ log "Initialize Root"
  handleAction Finalize =
    pure unit
  handleAction Add =
    H.modify_ \s ->
      { currentId: s.currentId + 1
      , slots: snoc s.slots s.currentId
      }
  handleAction (Remove id) =
    H.modify_ \s -> s { slots = filter (not <<< eq id) s.slots }
  handleAction Reverse =
    H.modify_ \s -> s { slots = reverse s.slots }
  handleAction (ReportRoot msg) =
    H.liftEffect $ log ("Root >>> " <> msg)

  listen :: Int -> Child.Message -> Action
  listen i = case _ of
    Child.Initialized -> ReportRoot ("Heard Initialized from child" <> show i)
    Child.Finalized -> ReportRoot ("Heard Finalized from child" <> show i)
    Child.Reported msg -> ReportRoot ("Re-reporting from child" <> show i <> ": " <> msg)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
