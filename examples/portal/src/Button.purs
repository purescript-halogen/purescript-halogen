module Example.Portal.Button (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Node as DOM
import Web.HTML (HTMLElement)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

type Input = HTMLElement

type State =
  { a :: { enabled :: Boolean, target :: HTMLElement }
  , b :: { enabled :: Boolean, target :: Maybe HTMLElement }
  }

data Action
  = Initialize
  | ToggleA
  | ToggleB

component :: forall q o. H.Component q Input o Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: Input -> State
initialState target = { a: { enabled: false, target }, b: { enabled: false, target: Nothing } }

render :: State -> H.ComponentHTML Action () Aff
render { a, b } =
  HH.div
    [ HP.attr (HH.AttrName "style") """padding: 10px; background-color: #ccc;""" ]
    [ HH.div
        [ ]
        [ HH.text "Some text, rendered in the component, which is rendered in the <body>." ]

    , HH.portal a.target do
        HH.div
          [ HP.attr (HH.AttrName "style") """padding: 10px; background-color: teal;""" ]
          [ HH.text "PORTAL A (targets <body>): "
          , HH.button
              [ HE.onClick \_ -> ToggleA ]
              [ HH.text $ if a.enabled then "On" else "Off" ]
          ]

    , case b.target of
        Nothing ->
          HH.text ""
        Just target ->
          HH.portal target do
            HH.div
              [ HP.attr (HH.AttrName "style") """padding: 10px; background-color: orange;""" ]
              [ HH.text "PORTAL B (targets <body><section>): "
              , HH.button
                  [ HE.onClick \_ -> ToggleB ]
                  [ HH.text $ if b.enabled then "On" else "Off" ]
              ]
    ]

handleAction ∷ forall o. Action → H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  ToggleA ->
    H.modify_ \st -> st { a { enabled = not st.a.enabled } }

  ToggleB ->
    H.modify_ \st -> st { b { enabled = not st.b.enabled } }

  Initialize -> do
    { a: { target } } <- H.get

    elem <- H.liftEffect do
      doc <- Window.document =<< HTML.window
      Document.createElement "section" (HTMLDocument.toDocument doc)

    node <- H.liftEffect $ DOM.appendChild (Element.toNode elem) (HTMLElement.toNode target)

    H.modify_ _ { b { target = HTMLElement.fromNode node } }
