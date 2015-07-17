module Examples.Ajax where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Aff.Class (MonadAff, liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.State.Class (modify)

import Css.Font (GenericFontFamily(..), fontFamily)
import Css.Geometry (height)
import Css.Size (px)
import Css.String (fromString)

import Data.DOM.Simple.Types (HTMLElement())
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML as H
import qualified Halogen.HTML.CSS as CSS
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Properties as P
import qualified Halogen.Themes.Bootstrap3 as B

import Network.HTTP.Affjax

-- | The state of the application
type State = { busy :: Boolean, code :: String, result :: Maybe String }

exampleCode :: String
exampleCode = """module Main where

import Prelude
import Control.Monad.Eff.Console (print)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = print (fact 20)
"""

initialState :: State
initialState = { busy: false, code: exampleCode, result: Nothing }

-- | Inputs to the state machine
data Input a
  = SetCode String a
  | MakeRequest String a
  | Init HTMLElement a
  | Final HTMLElement a

type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

ui :: forall eff p. ComponentFC State Input (Aff (AppEffects eff)) p
ui = componentFC render eval
  where

  render :: RenderFC State p Input
  render st =
    H.div [ P.class_ B.container ]
          $ [ H.h1 [ P.id_ "header" ] [ H.text "ajax example" ]
            , H.h2_ [ H.text "purescript code" ]
            , H.p_ [ H.textarea [ P.class_ B.formControl
                                , P.value st.code
                                , E.onValueInput (E.inputFC SetCode)
                                , CSS.style do
                                    fontFamily [] (pure monospace)
                                    height (px 200.0)
                                ] [] ]
            , H.p_ [ H.button [ P.classes [B.btn, B.btnPrimary]
                              , P.disabled st.busy
                              , E.onClick (E.inputFC_ (MakeRequest st.code))
                              ] [ H.text "Compile" ] ]
            , H.p_ [ H.text (if st.busy then "Working..." else "") ]
            ] ++ flip foldMap st.result \js ->
            [ H.div_ {-[ H.Initializer (\el -> actionFC (Init el))
                    , H.Finalizer (\el -> actionFC (Final el))
                    ]-}
                    [ H.h2_ [ H.text "compiled javascript" ]
                    , H.pre_ [ H.code_ [ H.text js ] ]
                    ]
            ]

  monospace :: GenericFontFamily
  monospace = GenericFontFamily $ fromString "monospace"

  eval :: Eval Input State (Aff (AppEffects eff))
  eval (SetCode code next) = modify (_ { code = code }) $> next
  eval (MakeRequest code next) = do
    modify (_ { busy = true })
    result <- handler code
    modify (_ { busy = false, result = Just result })
    pure next
  eval (Init _ next) = liftEff (log "UI initialized") $> next
  eval (Final _ next) = liftEff (log "UI finalized") $> next

-- | Handle a request to an external service
handler :: forall eff m. (Monad m, MonadAff (ajax :: AJAX | eff) m) => String -> m String
handler code = do
  result <- liftAff $ post "http://try.purescript.org/compile/text" code
  let response = result.response
  return case readProp "js" response <|> readProp "error" response of
    Right js -> js
    Left _ -> "Invalid response"

main :: Eff (AppEffects ()) Unit
main = launchAff $ do
  { node: node } <- runUI ui initialState
  appendToBody node
