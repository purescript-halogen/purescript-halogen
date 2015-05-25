module Main where

import Data.Void
import Data.Tuple
import Data.Either

import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
newtype State = State { on :: Boolean }

-- | Inputs to the state machine
data Input = ToggleState

ui :: forall m eff. (Applicative m) => Component m Input Input
ui = render <$> stateful (State { on: false }) update
  where
  render :: State -> H.HTML (m Input)
  render (State s) = H.div_
    [ H.h1_ [ H.text "Toggle Button" ]
    , H.button [ A.onClick (A.input_ ToggleState) ] 
               [ H.text (if s.on then "On" else "Off") ]
    ]    
      
  update :: State -> Input -> State
  update (State s) ToggleState = State { on: not s.on }

main = do
  Tuple node _ <- runUI ui
  appendToBody node
