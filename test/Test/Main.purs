module Test.Main where

import Prelude

import Control.Monad.Free
import Control.Monad.Eff.Console (log)
import Control.Monad.Rec.Class
import Control.Monad.State.Class (modify, gets)
import Control.Monad.State.Trans

import Data.Functor
import Data.Coyoneda
import Data.Identity
import Data.Inject
import Data.Void

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.Properties as P

data Input a
  = ClickIncrement a
  | ClickDecrement a
  | UpdateValue String a
  | GetValue (String -> a)

type State = { count :: Int, value :: String }

counterComponent :: forall g. (Monad g, MonadRec g) => ComponentFC State Input g Void
counterComponent = component render query
  where

  render :: State -> H.HTML Void (FreeC Input Unit)
  render st = H.div_ [ H.p_ [ H.text (show st.count) ]
                     , H.button [ E.onClick (\_ -> E.stopPropagation $> actionFC ClickIncrement) ]
                                [ H.text "Up" ]
                     , H.button [ E.onClick (E.inputFC_ ClickDecrement) ]
                                [ H.text "Down" ]
                     , H.p_ [ H.input [ P.value st.value
                                      , E.onValueInput (E.inputFC UpdateValue)
                                      ]
                            ]
                     , H.p_ [ H.text $ "Value: " ++ st.value ]
                     ]

  query :: forall i. FreeC Input i -> StateT State g i
  query = runFreeCM eval

  eval :: Natural Input (StateT State g)
  eval (ClickIncrement next) = modify (\st -> st { count = st.count + 1 }) $> next
  eval (ClickDecrement next) = modify (\st -> st { count = st.count - 1 }) $> next
  eval (UpdateValue v next) = modify (_ { value = v }) $> next
  eval (GetValue k) = gets (_.value) >>= pure <<< k

import Control.Monad.Eff
import DOM
import Control.Bind
import Data.Tuple
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

main :: Eff (HalogenEffects ()) Unit
main = do
  ui <- runUI counterComponent { count: 0, value: "" }
  appendToBody ui.node
  setInterval globalWindow 1000.0 $ ui.driver (actionFC ClickDecrement)
  setTimeout globalWindow 5000.0 $ do
    val <- ui.driver (requestFC GetValue)
    log val
  pure unit
