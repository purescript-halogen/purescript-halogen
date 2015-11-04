module Example.Lifecycle where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

-- | The state of the application
type State = { events :: Array String, on :: Boolean }

initialState :: State
initialState = { events: [], on: true }

-- | Queries to the state machine
data Query a = ToggleState a
             | AddEvent String a

ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "Hide" else "Show") ]
      , if state.on
        then
          H.span
            [ P.initializer \_ -> action (AddEvent "Initialized")
            , P.finalizer \_ -> action (AddEvent "Finalized")
            ]
            [ H.text "Hello, World!" ]
        else
          H.text ""
      , H.ul_ (map (\event -> H.li_ [ H.text event ]) state.events)
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> state { on = not state.on })
    pure next
  eval (AddEvent event next) = do
    modify (\state -> state { events = state.events <> [event] })
    pure next

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
