module Example.Ajax where

import Data.Void
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Foldable (foldMap)

import qualified Data.String as S

import qualified Data.StrMap as StrMap

import Control.Functor (($>))
import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import qualified Halogen.Themes.Bootstrap3 as B

foreign import data HTTP :: !

exampleCode :: String
exampleCode = S.joinWith "\n"
  [ "module Main where"
  , ""
  , "fact :: Number -> Number"
  , "fact 0 = 1"
  , "fact n = n * fact (n - 1)"
  ]

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node
  
foreign import compile
  "function compile(code) {\
  \  return function(k) {\
  \    return function() {\
  \      var xhr = new XMLHttpRequest();\
  \      xhr.onreadystatechange = function(){\
  \        if (xhr.readyState === 4) {\
  \          k(xhr.responseText)();\
  \        }\
  \      };\
  \      xhr.open('POST', 'http://try.purescript.org/compile/text', true);\
  \      xhr.send(code);\
  \    };\
  \  };\
  \}" :: forall eff. String -> (String -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit
  
-- | The state of the application
data State = State Boolean String (Maybe String)

-- | Inputs to the state machine
data Input 
  = SetBusy
  | SetCode String
  | SetResult String
  
-- | Requests to external services
data Request = CompileRequest String
  
ui :: forall eff a. SF1 Input (H.HTML a (Either Input Request))
ui = view <$> stateful (State false exampleCode Nothing) update
  where
  view :: State -> H.HTML a (Either Input Request)
  view (State busy code result) = 
    H.div (A.class_ B.container) $
          [ H.h1 (A.id_ "header") [ H.text "ajax example" ]
          , H.h2_ [ H.text "purescript code" ]
          , H.p_ [ H.textarea (A.class_ B.formControl 
                               <> A.value code 
                               <> A.onInput (pure <<< Left <<< SetCode)
                               <> A.style (StrMap.fromList [ Tuple "font-family" "monospace"
                                                           , Tuple "height" "200px"
                                                           ])
                              ) [] ]
          , H.p_ [ H.button (A.classes [B.btn, B.btnPrimary] <> A.disabled busy <> A.onclick (\_ -> pure (Right (CompileRequest code)))) [ H.text "Compile" ] ]
          , H.p_ [ H.text (if busy then "Working..." else "") ]
          ] ++ flip foldMap result \js ->
          [ H.h2_ [ H.text "compiled javascript" ]
          , H.pre_ [ H.code_ [ H.text js ] ]
          ]

  update :: State -> Input -> State
  update (State _ code rslt) SetBusy = State true code rslt
  update (State busy _ _) (SetCode code) = State busy code Nothing
  update (State busy code _) (SetResult rslt) = State false code (Just rslt)

-- | Handle a request to an external service
handleRequest :: forall eff. Handler Request Input (http :: HTTP | eff)
handleRequest (CompileRequest code) k = do
  k SetBusy
  compile code \response -> do
    k (SetResult response)
  
main = do
  Tuple node driver <- runUIEff ui absurd handleRequest
  appendToBody node
