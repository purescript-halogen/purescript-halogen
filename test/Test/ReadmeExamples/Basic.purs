module Test.ReadmeExamples.Basic where

import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

-- | The state of the component
type State = { on :: Boolean }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
myComponent :: forall g. Component State Query g
myComponent = component render eval
  where

  render :: Render State Query
  render state =
    H.div_ [ H.button [ E.onClick (E.input_ ToggleState) ]
                      [ H.text (if state.on then "On" else "Off") ]
           ]

  eval :: Eval Query State Query g
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next
  eval (GetState continue) = do
    value <- gets _.on
    pure (continue value)
