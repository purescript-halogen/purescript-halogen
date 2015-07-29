module Main where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free (Free(), liftF)
import Halogen.Query.StateF (StateF(), modify, get)

import Data.DOM.Simple.Document (createElement)
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Window (document, globalWindow)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, right)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable(), toNullable)

import DOM (DOM())

import Halogen
import Halogen.Component.Widget
import qualified Halogen.HTML as H
-- import qualified Halogen.HTML.Properties as P
-- import qualified Halogen.HTML.Events as E

import Ace.Types (ACE(), Editor(), EditSession())
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session

type AceState = Maybe Editor

data AceInput a = ChangeText String a

ace :: forall p eff. WidgetFC AceState AceInput (Aff (ace :: ACE | eff)) p
ace = widgetFC_ init eval

  where

  init :: WidgetHandler AceState (Aff (ace :: ACE | eff))
  init el = do
    editor <- liftEffW $ Ace.editNode el Ace.ace
    modify $ const $ Just editor
    session <- liftEffW $ Editor.getSession editor
    liftEffW $ Editor.setTheme "ace/theme/monokai" editor
--     -- Set up event handlers
--     Editor.onCopy editor (driver <<< TextCopied)
--     Session.onChange session do
--       text <- Editor.getValue editor
--       driver $ TextChanged text

  eval :: Eval AceInput AceState (Aff (ace :: ACE | eff))
  eval (ChangeText text next) = do
    state <- get
    case state of
      Nothing -> pure next
      Just editor -> do
        current <- liftEffW $ Editor.getValue editor
        when (text /= current) $ void $ liftEffW $ Editor.setValue text Nothing editor
        pure next

-- | The application state, which stores the current text, and the string most recently
-- | copied to the clipboard.
data State = State { text :: String, copied :: Maybe String }

-- | The type of inputs to the component.
data Input a = CopyText (String -> a)

-- ui :: forall eff p. ComponentF State Input (Aff (ace :: ACE | eff)) p
-- ui = render <$> stateful (State "" Nothing) update
--   where
--   render :: State -> H.HTML (m Input)
--   render (State text copied) =
--     H.div_ -- [ A.class_ B.container ]
--           [ H.h1_ [ H.text "ace editor" ]
--           , H.div_ [ H.p_ [ H.button [ A.classes [] -- [ B.btn, B.btnPrimary ]
--                                      , A.onClick (A.input_ (TextChanged ""))
--                                      ] [ H.text "Clear" ]
--                           ]
--                    , H.p_ [ H.text (maybe "" ("Text copied: " <>) copied) ]
--                    ]
--           , H.div [ dataAceText text ] []
--           ]

--   update :: State -> Input -> State
--   update (State text _) (TextCopied copied) = State text (Just copied)
--   update (State _ copied) (TextChanged text) = State text copied

-- main = do
--   -- A RefVal is used to manage widget state.
--   -- In practice, this data store might need to be more complex, mapping component IDs to individual states.
--   editorRef <- newRef Nothing

--   Tuple node driver <- runUIWith ui (updateAce editorRef)

--   doc <- document globalWindow
--   b <- body doc
--   b `appendChild` node

--   -- Set up the Ace editor
--   els <- querySelector "[data-ace-text]" b
--   for_ els \el -> do
--     -- Setup the Ace editor
--     editor <- Ace.editNode el ace
--     session <- Editor.getSession editor
--     writeRef editorRef (Just editor)
--     Editor.setTheme "ace/theme/monokai" editor
--     -- Set up event handlers
--     Editor.onCopy editor (driver <<< TextCopied)
--     Session.onChange session do
--       text <- Editor.getValue editor
--       driver $ TextChanged text

