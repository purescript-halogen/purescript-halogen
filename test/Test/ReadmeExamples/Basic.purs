module Test.ReadmeExamples.Basic where

import Prelude
import Data.Maybe (Maybe(..))
import Halogen (ComponentDSL, ComponentHTML, Component, component, gets, modify)
import Halogen.HTML as H
import Halogen.HTML.Events as E

-- | The state of the component
type State = { on :: Boolean }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
myComponent :: forall m. Component H.HTML Query Unit Void m
myComponent =
  component
    { initialState: const { on: false }
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Query ~> ComponentDSL State Query Void m
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next
  eval (GetState reply) = do
    value <- gets _.on
    pure (reply value)
