module Test.Main where

import Data.Maybe
import Data.Tuple
import Data.Either

import Debug.Trace

import Control.Monad (when)
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class
import Control.Monad.Aff

import DOM

import Halogen
import Halogen.Signal


import qualified Halogen.Mixin.UndoRedo as U

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

import qualified Halogen.Themes.Bootstrap3 as B

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

-- | The state of the application is the most recent error (if appropriate) and a counter 
data State = State (Maybe Error) Number

-- | Inputs to the state machine:
-- | 
-- | - `OnError e` - respond to the error `e`
-- | - `SetCounter n` - a request to update the counter to value `n`
data Input 
  = OnError Error    
  | SetCounter Number
  | Undo
  | Redo
  
instance inputSupportsErrors :: SupportsErrors Input where
  liftError = OnError
  
instance inputSupportsUndoRedo :: U.SupportsUndoRedo Input where
  fromUndoRedo U.Undo = Undo
  fromUndoRedo U.Redo = Redo
  toUndoRedo Undo = Just U.Undo
  toUndoRedo Redo = Just U.Redo
  toUndoRedo _ = Nothing

-- | External requests: 
-- | 
-- | - `AddService n m` - send a request to an external service to add the numbers `n` and `m`
data Request = AddService Number Number

-- | The UI is a state machine, consuming errors and inputs, and generating HTML documents which generate
-- | external service requests of type `Request`.
ui :: forall eff. SF1 Input (H.HTML (Either Input Request))
ui = view <$> stateful (U.undoRedoState (State Nothing 0)) (U.withUndoRedo update)
  where
  view :: U.UndoRedoState State -> H.HTML (Either Input Request)
  view st = 
    case U.getState st of
      State err n -> 
        H.div [ A.class_ B.container ] 
              [ H.h1 [ A.id_ "header" ] [ H.code_ [H.text "purescript-halogen"], H.text " demo" ]
              , H.p_ [ H.text "Click the buttons to modify the state of the view." ]
              , H.p_ [ H.text (maybe "" message err) ]
              , H.p_ [ H.text ("Current state: " <> show n) ]
              , H.p [ A.class_ B.btnGroup ]
                    [ H.button [ A.classes [ B.btn, B.btnPrimary ], A.onclick (const (Right (AddService n 1))) ] [ H.text "Increment" ]
                    , H.button [ A.classes [ B.btn, B.btnPrimary ], A.onclick (const (Right (AddService n (-1)))) ] [ H.text "Decrement" ]
                    , H.button [ A.class_ B.btn, A.enabled (U.canUndo st), A.onclick (const (Left Undo)) ] [ H.text "Undo" ]
                    , H.button [ A.class_ B.btn, A.enabled (U.canRedo st), A.onclick (const (Left Redo)) ] [ H.text "Redo" ]
                    ]
              ]

  update :: State -> Input -> State
  update (State _ n) (OnError err) = State (Just err) n
  update _ (SetCounter n) = State Nothing n

-- | This function handles external service requests.
-- |
-- | We simulate an AJAX request using the `Aff` monad. The service throws an error if the first input is too large.
handler :: forall eff. Request -> Aff (trace :: Trace | eff) Input
handler (AddService n m) = do
  liftEff $ trace $ "Adding " <> show n <> " and " <> show m
  when (n >= 10) $ throwError $ error "Input too large"
  return $ SetCounter $ n + m

main = do
  node <- runUIAff ui handler
  appendToBody node
