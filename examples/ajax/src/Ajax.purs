module Example.Ajax where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Free (liftFI)

import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), post)

-- | The state of the application
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

-- | Inputs to the state machine.
data Input a
  = SetCode String a
  | MakeRequest String a

-- | The effects used in the app.
type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component State Input (Aff (AppEffects eff))
ui = component render eval
  where

  render :: Render State Input
  render st =
    H.div_ $ [ H.h1_ [ H.text "ajax example / trypurescript" ]
             , H.h2_ [ H.text "purescript input:" ]
             , H.p_ [ H.textarea [ P.value st.code
                                 , E.onValueInput (E.input SetCode)
                                 ]
                    ]
             , H.p_ [ H.button [ P.disabled st.busy
                               , E.onClick (E.input_ (MakeRequest st.code))
                               ]
                               [ H.text "Compile" ]
                    ]
             , H.p_ [ H.text (if st.busy then "Working..." else "") ]
             ]
             ++ flip foldMap st.result \js ->
                [ H.div_ [ H.h2_ [ H.text "javascript output:" ]
                         , H.pre_ [ H.code_ [ H.text js ] ]
                         ]
                ]

  eval :: Eval Input State Input (Aff (AppEffects eff))
  eval (SetCode code next) = modify (_ { code = code, result = Nothing :: Maybe String }) $> next
  eval (MakeRequest code next) = do
    modify (_ { busy = true })
    result <- liftFI (fetchJS code)
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
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  appendToBody app.node
