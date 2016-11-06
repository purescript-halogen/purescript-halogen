module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

import Data.HTTP.Method (Method(GET))
import Network.HTTP.Affjax (AJAX, defaultRequest, affjax)

-- | The state of the component.
type State = { busy :: Boolean, result :: Maybe String }

initialState :: State
initialState = { busy: false, result: Nothing }

-- | The component query algebra.
data Query a = MakeRequest a

-- | The effects used in the app.
type AppEffects eff = H.HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. H.Component State Query (Aff (AppEffects eff))
ui = H.component { render, eval }
  where

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.h1_
          [ HH.text "ajax example / httpbin" ]
      , HH.p_
          [ HH.button
              [ HP.disabled st.busy
              , HE.onClick (HE.input_ MakeRequest)
              ]
              [ HH.text "GET" ]
          ]
      , HH.p_
          [ HH.text (if st.busy then "Working..." else "") ]
      ]
      <> flip foldMap st.result \s ->
          [ HH.div_
              [ HH.h2_
                  [ HH.text "response headers.User-Agent:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text s] ]
              ]
          ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (AppEffects eff))
  eval (MakeRequest next) = do
    H.modify (_ { busy = true })
    result <- H.fromAff (fetchJS unit)
    H.modify (_ { busy = false, result = Just result })
    pure next

-- | GET JSON and pull fields from the result.
fetchJS :: forall eff. Unit -> Aff (ajax :: AJAX | eff) String
fetchJS unit = do
  let req = defaultRequest { method = Left GET
                           , url = "http://httpbin.org/get"
                           }
  result <- affjax req
  let response = result.response

  pure $ case runExcept $ readProp "headers" response of
    Right headers ->
      case runExcept $ readProp "User-Agent" headers of
        Right ua -> ua 
        Left _ -> "Unexpected response"
    Left _ -> "Unexpected response"

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui initialState body

