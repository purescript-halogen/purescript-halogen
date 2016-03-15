module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), post)

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
type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render st =
    H.div_ $
      [ H.h1_
          [ H.text "ajax example / trypurescript" ]
      , H.h2_
          [ H.text "purescript input:" ]
      , H.p_
          [ H.textarea
              [ P.value st.code
              , E.onValueInput (E.input SetCode)
              ]
          ]
      , H.p_
          [ H.button
              [ P.disabled st.busy
              , E.onClick (E.input_ (MakeRequest st.code))
              ]
              [ H.text "Compile" ]
          ]
      , H.p_
          [ H.text (if st.busy then "Working..." else "") ]
      ]
      ++ flip foldMap st.result \js ->
          [ H.div_
              [ H.h2_
                  [ H.text "javascript output:" ]
              , H.pre_
                  [ H.code_ [ H.text js ] ]
              ]
          ]

  eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
  eval (SetCode code next) = modify (_ { code = code, result = Nothing :: Maybe String }) $> next
  eval (MakeRequest code next) = do
    modify (_ { busy = true })
    result <- fromAff (fetchJS code)
    modify (_ { busy = false, result = Just result })
    pure next

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS code = do
  result <- post "http://try.purescript.org/compile/text" code
  let response = result.response
  return case readProp "js" response <|> readProp "error" response of
    Right js -> js
    Left _ -> "Invalid response"

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
