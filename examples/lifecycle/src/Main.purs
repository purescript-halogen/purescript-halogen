module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Array (snoc, filter, reverse)
import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct, coproduct)

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

type UIEff eff = Aff (console :: CONSOLE | eff)

type StateP eff = H.ParentState State (Child.StateP eff) Query Child.QueryP (UIEff eff) Int

type QueryP = Coproduct Query (H.ChildF Int Child.QueryP)

ui :: forall eff. H.Component (StateP eff) QueryP (UIEff eff)
ui = H.lifecycleParentComponent
  { render: render
  , eval: eval
  , peek: Just (peek <<< H.runChildF)
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: State -> H.ParentHTML (Child.StateP eff) Query Child.QueryP (UIEff eff) Int
  render state =
    HH.div_
      [ HH.button
          [ HE.onClick (HE.input_ Add) ]
          [ HH.text "Add" ]
      , HH.button
          [ HE.onClick (HE.input_ Reverse) ]
          [ HH.text "Reverse" ]
      , HH.ul_ $ flip map state.slots \id ->
          HH.li
            [ HP.key (show id) ]
            [ HH.button
                [ HE.onClick (HE.input_ $ Remove id) ]
                [ HH.text "Remove" ]
            , HH.slot id \_ -> { component: Child.child, initialState: H.parentState id }
            ]
      ]

  eval :: Query ~> H.ParentDSL State (Child.StateP eff) Query Child.QueryP (UIEff eff) Int
  eval (Initialize next) = do
    H.fromAff $ log "Initialize Root"
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

  peek :: forall x. Child.QueryP x -> H.ParentDSL State (Child.StateP eff) Query Child.QueryP (UIEff eff) Int Unit
  peek = coproduct peek' (const (pure unit))
    where
    peek' (Child.Initialize _) = do
      H.fromAff $ log "Peeked child init"
      pure unit
    peek' (Child.Finalize _) = do
      -- This will never happen, finalizers are not peek-able
      H.fromAff $ log "Peeked child finalize"
      pure unit
    peek' _ = pure unit

main :: Eff (H.HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui (H.parentState initialState) body
