module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Array (snoc, filter, reverse)
import Data.Maybe (Maybe(..))
import Data.Lazy (defer)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

import Child as Child

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

type UIEff eff = Aff (console :: CONSOLE | eff)

ui :: forall eff. H.Component HH.HTML Query Void (UIEff eff)
ui = H.lifecycleParentComponent
  { render: render
  , eval: eval
  , initialState: initialState
  -- , peek: Just (peek <<< H.runChildF)
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: State -> H.ParentHTML Query Child.Query Int (UIEff eff)
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick (HE.input_ Add) ]
          [ HH.text "Add" ]
      , HH.button
          [ HE.onClick (HE.input_ Reverse) ]
          [ HH.text "Reverse" ]
      , HH.ul_ $ flip map state.slots \sid ->
          HH.li
            [ HP.key (show sid) ]
            [ HH.button
                [ HE.onClick (HE.input_ $ Remove sid) ]
                [ HH.text "Remove" ]
            , HH.slot sid (defer \_ -> Child.child sid) (listen sid)
            ]
      ]

  eval :: Query ~> H.ParentDSL State Query Child.Query Int Void (UIEff eff)
  eval (Initialize next) = do
    H.liftAff $ log "Initialize Root"
    pure next
  eval (Finalize next) = do
    pure next
  eval (Add next) = do
    H.modify \s ->
      { currentId: s.currentId + 1
      , slots: snoc s.slots s.currentId
      }
    pure next
  eval (Remove id next) = do
    H.modify \s -> s { slots = filter (not <<< eq id) s.slots }
    pure next
  eval (Reverse next) = do
    H.modify \s -> s { slots = reverse s.slots }
    pure next
  eval (ReportRoot msg next) = do
    H.liftAff $ log ("Root >>> " <> msg)
    pure next

  listen :: Int -> Child.Message -> Maybe (Query Unit)
  listen i = Just <<< case _ of
    Child.Initialized -> H.action $ ReportRoot ("Heard Initialized from child" <> show i)
    Child.Finalized -> H.action $ ReportRoot ("Heard Finalized from child" <> show i)
    Child.Refd -> H.action $ ReportRoot ("Heard Refd from child" <> show i)
    Child.Reported msg -> H.action $ ReportRoot ("Re-reporting from child" <> show i <> ": " <> msg)

  -- peek :: forall x. Child.Query' x -> H.ParentDSL State Query Child.Query' (UIEff eff) Int Unit
  -- peek = coproduct peek' (const (pure unit))
  --   where
  --   peek' (Child.Initialize _) = do
  --     H.liftAff $ log "Peeked child init"
  --     pure unit
  --   peek' (Child.Finalize _) = do
  --     -- This will never happen, finalizers are not peek-able
  --     H.liftAff $ log "Peeked child finalize"
  --     pure unit
  --   peek' _ = pure unit

main :: Eff (H.HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui body
