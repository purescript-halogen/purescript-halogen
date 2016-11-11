module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.VirtualDOM.Driver (runUI)

import Network.HTTP.Affjax (AJAX, post)

-- | The state of the component.
type State = { busy :: Boolean, code :: String, result :: Maybe String }

initialState :: State
initialState = { busy: false, code: exampleCode, result: Nothing }

exampleCode :: String
exampleCode = """module Main where

import Prelude
import Control.Monad.Eff.Console (print)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = print (fact 20)
"""

-- | The component query algebra.
data Query a
  = SetCode String a
  | MakeRequest String a

-- | The effects used in the app.
type AppEffects eff = H.HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. H.Component HH.HTML Query Void (Aff (AppEffects eff))
ui = H.component { render, eval, initialState }
  where

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.h1_
          [ HH.text "ajax example / trypurescript" ]
      , HH.h2_
          [ HH.text "purescript input:" ]
      , HH.p_
          [ HH.textarea
              [ HP.value st.code
              , HE.onValueInput (HE.input SetCode)
              ]
          ]
      , HH.p_
          [ HH.button
              [ HP.disabled st.busy
              , HE.onClick (HE.input_ (MakeRequest st.code))
              ]
              [ HH.text "Compile" ]
          ]
      , HH.p_
          [ HH.text (if st.busy then "Working..." else "") ]
      ]
      <> flip foldMap st.result \js ->
          [ HH.div_
              [ HH.h2_
                  [ HH.text "javascript output:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text js ] ]
              ]
          ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (AppEffects eff))
  eval (SetCode code next) = H.modify (_ { code = code, result = Nothing :: Maybe String }) $> next
  eval (MakeRequest code next) = do
    H.modify (_ { busy = true })
    result <- H.liftAff (fetchJS code)
    H.modify (_ { busy = false, result = Just result })
    pure next

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS code = do
  result <- post "http://try.purescript.org/compile/text" code
  let response = result.response
  pure case runExcept $ readProp "js" response <|> readProp "error" response of
    Right js -> js
    Left _ -> "Invalid response"

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
