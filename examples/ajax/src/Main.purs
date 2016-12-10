module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax as AX

-- | The component state record.
type State =
  { busy :: Boolean
  , username :: String
  , result :: Maybe String
  }

-- | The component query algebra.
data Query a
  = SetUsername String a
  | MakeRequest a

-- | The effects used within the component.
type Effects eff = (ajax :: AX.AJAX | eff)

-- | The state value to use when the component is initialized.
initialState :: State
initialState = { busy: false, username: "", result: Nothing }

-- | The component renderer.
render :: State -> H.ComponentHTML Query
render st =
  HH.div_ $
    [ HH.h1_ [ HH.text "Lookup GitHub user" ]
    , HH.label_
        [ HH.div_ [ HH.text "Enter username:" ]
        , HH.input
            [ HP.value st.username
            , HE.onValueInput (HE.input SetUsername)
            ]
        ]
    , HH.button
        [ HP.disabled st.busy
        , HE.onClick (HE.input_ MakeRequest)
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text (if st.busy then "Working..." else "") ]
    , HH.div_
        case st.result of
          Nothing -> []
          Just res ->
            [ HH.h2_
                [ HH.text "Response:" ]
            , HH.pre_
                [ HH.code_ [ HH.text res ] ]
            ]
    ]

-- | The component query evaluator.
eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
eval = case _ of
  SetUsername username next -> do
    H.modify (_ { username = username, result = Nothing :: Maybe String })
    pure next
  MakeRequest next -> do
    username <- H.gets _.username
    H.modify (_ { busy = true })
    response <- H.liftAff $ AX.get ("https://api.github.com/users/" <> username)
    H.modify (_ { busy = false, result = Just response.response })
    pure next

-- | The main UI component.
ui :: forall eff. H.Component HH.HTML Query Void (Aff (Effects eff))
ui = H.component { render, eval, initialState }

-- | Run the app.
main :: Eff (H.HalogenEffects (Effects ())) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
