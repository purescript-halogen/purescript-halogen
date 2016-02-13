module Example.Lifecycle where

import Prelude

import Control.Monad.Aff (runAff, Aff())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (throwException)

import Data.Array (snoc, filter, reverse)
import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct())
import Data.Functor.Aff (liftAff)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import qualified Example.Lifecycle.Child as Child

type State =
  { currentId :: Int
  , slots :: Array Int
  }

initialState :: State
initialState =
  { currentId: 0
  , slots: []
  }

data Query a = Initialize a
             | Finalize a
             | Add a
             | Reverse a
             | Remove Int a


type UIEff eff = Aff (console :: CONSOLE | eff)

type StateP eff = InstalledState State (Child.StateP eff) Query Child.QueryP (UIEff eff) Int

type QueryP = Coproduct Query (ChildF Int Child.QueryP)

ui :: forall eff. Component (StateP eff) QueryP (UIEff eff)
ui =
  parentComponentSpec
    { render: render
    , eval: eval
    , initializer: Just (action Initialize)
    , finalizer: Just (action Finalize)
    }
  where

  render :: State -> ParentHTML (Child.StateP eff) Query Child.QueryP (UIEff eff) Int
  render state =
    H.div_
      [ H.button
          [ E.onClick (E.input_ Add) ]
          [ H.text "Add" ]
      , H.button
          [ E.onClick (E.input_ Reverse) ]
          [ H.text "Reverse" ]
      , H.ul_ $ flip map state.slots \id ->
          H.li [ P.key (show id) ]
            [ H.button
                [ E.onClick (E.input_ $ Remove id) ]
                [ H.text "Remove" ]
            , H.slot id \_ -> { component: Child.child, initialState: installedState id }
            ]
      ]

  eval :: Natural Query (ParentDSL State (Child.StateP eff) Query Child.QueryP (UIEff eff) Int)
  eval (Initialize next) = do
    liftAff $ log "Initialize Root"
    pure next
  eval (Finalize next) = do
    pure next
  eval (Add next) = do
    modify \s ->
      { currentId: s.currentId + 1
      , slots: snoc s.slots s.currentId
      }
    pure next
  eval (Remove id next) = do
    modify \s -> s { slots = filter (not <<< eq id) s.slots }
    pure next
  eval (Reverse next) = do
    modify \s -> s { slots = reverse s.slots }
    pure next

main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui (installedState initialState)
  onLoad $ appendToBody app.node
