module Example.RawHTML.Main where

import Prelude

import Data.Array (snoc, length)
import Data.String (joinWith)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type State =
  { counter :: Int
  , showRaw :: Boolean
  , items :: Array String
  }

data Action
  = Increment
  | Decrement
  | ToggleRaw
  | AddItem

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { counter: 0
      , showRaw: true
      , items: [ "<li>Item 1</li>", "<li>Item 2</li>" ]
      }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h1_ [ HH.text "Raw HTML Demo" ]

    -- Test 1: Static raw HTML
    , HH.div [ HP.id "test-static" ]
        [ HH.h2_ [ HH.text "Test 1: Static Raw HTML" ]
        , HH.rawHTML "<p><strong>This is raw HTML</strong> with <em>formatting</em></p>"
        ]

    -- Test 2: Conditional raw HTML (toggle on/off)
    , HH.div [ HP.id "test-toggle" ]
        [ HH.h2_ [ HH.text "Test 2: Conditional Raw HTML" ]
        , HH.button [ HE.onClick \_ -> ToggleRaw, HP.id "btn-toggle" ]
            [ HH.text (if state.showRaw then "Hide Raw HTML" else "Show Raw HTML") ]
        , if state.showRaw
            then HH.rawHTML "<div id='raw-conditional'><span style='color: green;'>Visible raw HTML</span></div>"
            else HH.p_ [ HH.text "Raw HTML is hidden" ]
        ]

    -- Test 3: Dynamic raw HTML (changes based on state)
    , HH.div [ HP.id "test-dynamic" ]
        [ HH.h2_ [ HH.text "Test 3: Dynamic Raw HTML (counter)" ]
        , HH.button [ HE.onClick \_ -> Decrement, HP.id "btn-dec" ] [ HH.text "-" ]
        , HH.button [ HE.onClick \_ -> Increment, HP.id "btn-inc" ] [ HH.text "+" ]
        , HH.rawHTML ("<div id='raw-counter'><span>Count: " <> show state.counter <> "</span></div>")
        ]

    -- Test 4: Raw HTML mixed with regular Halogen elements
    , HH.div [ HP.id "test-mixed" ]
        [ HH.h2_ [ HH.text "Test 4: Mixed Content" ]
        , HH.p_ [ HH.text "Regular Halogen paragraph before" ]
        , HH.rawHTML "<p id='raw-mixed'>Raw HTML paragraph in the middle</p>"
        , HH.p_ [ HH.text "Regular Halogen paragraph after" ]
        ]

    -- Test 5: Raw HTML list that grows
    , HH.div [ HP.id "test-list" ]
        [ HH.h2_ [ HH.text "Test 5: Growing Raw HTML List" ]
        , HH.button [ HE.onClick \_ -> AddItem, HP.id "btn-add" ] [ HH.text "Add Item" ]
        , HH.rawHTML ("<ul id='raw-list'>" <> joinWith "" state.items <> "</ul>")
        ]

    -- Test 6: Nested HTML structure
    , HH.div [ HP.id "test-nested" ]
        [ HH.h2_ [ HH.text "Test 6: Nested Raw HTML" ]
        , HH.rawHTML "<div id='raw-nested'><table><tr><td>Cell 1</td><td>Cell 2</td></tr><tr><td>Cell 3</td><td>Cell 4</td></tr></table></div>"
        ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment -> H.modify_ \s -> s { counter = s.counter + 1 }
  Decrement -> H.modify_ \s -> s { counter = s.counter - 1 }
  ToggleRaw -> H.modify_ \s -> s { showRaw = not s.showRaw }
  AddItem -> H.modify_ \s ->
    s { items = snoc s.items ("<li>Item " <> show (length s.items + 1) <> "</li>") }
