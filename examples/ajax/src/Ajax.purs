module Example.Ajax where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (liftFI)
import qualified Control.Monad.Eff.Console as C

import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Query.StateF (modify)
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Properties as P

import Network.HTTP.Affjax

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
  | Init HTMLElement a
  | Final HTMLElement a

-- | The effects used in the app.
type AppEffects = HalogenEffects (ajax :: AJAX)

-- | The definition for the app's main UI component.
ui :: forall eff p. ComponentFC State Input (Aff AppEffects) p
ui = componentFC render eval
  where

  render :: RenderFC State p Input
  render st =
    H.div_ $ [ H.h1_ [ H.text "ajax example / trypurescript" ]
             , H.h2_ [ H.text "purescript input:" ]
             , H.p_ [ H.textarea [ P.value st.code
                                 , E.onValueInput (E.inputFC SetCode)
                                 ]
                    ]
             , H.p_ [ H.button [ P.disabled st.busy
                               , E.onClick (E.inputFC_ (MakeRequest st.code))
                               ]
                               [ H.text "Compile" ]
                    ]
             , H.p_ [ H.text (if st.busy then "Working..." else "") ]
             ]
             ++ flip foldMap st.result \js ->
                [ H.div [ initializer
                        , finalizer
                        ]
                        [ H.h2_ [ H.text "javascript output:" ]
                        , H.pre_ [ H.code_ [ H.text js ] ]
                        ]
                ]

  initializer = H.Initializer (\el -> actionFC (Init el))
  finalizer = H.Finalizer (\el -> actionFC (Final el))

  eval :: Eval Input State (Aff AppEffects)
  eval (SetCode code next) = modify (_ { code = code, result = Nothing :: Maybe String }) $> next
  eval (MakeRequest code next) = do
    modify (_ { busy = true })
    result <- liftFI (fetchJS code)
    modify (_ { busy = false, result = Just result })
    pure next
  eval (Init _ next) = liftFI (log "Compile output initialized") $> next
  eval (Final _ next) = liftFI (log "Compile output finalized") $> next

-- | Log a message to the console via Aff.
log :: forall eff. String -> Aff (console :: C.CONSOLE | eff) Unit
log = liftEff <<< C.log

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS code = do
  result <- post "http://try.purescript.org/compile/text" code
  let response = result.response
  return case readProp "js" response <|> readProp "error" response of
    Right js -> js
    Left _ -> "Invalid response"

-- | Run the app.
main :: Eff AppEffects Unit
main = launchAff $ do
  app <- runUI ui initialState
  appendToBody app.node
