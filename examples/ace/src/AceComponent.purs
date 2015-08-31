module AceComponent where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE(), log)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement())

import Halogen
import qualified Halogen.HTML as H

import Ace.Types (ACE(), Editor())
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session

type AceState = { editor :: Maybe Editor }

data AceInput a
  = Init HTMLElement a
  | ChangeText String a

initAceState :: AceState
initAceState = { editor: Nothing }

type AceEffects eff = (ace :: ACE, avar :: AVAR, console :: CONSOLE | eff)

ace :: forall p eff. String -> Component AceState AceInput (Aff (AceEffects eff)) p
ace key = component render eval
  where

  render :: Render AceState AceInput p
  render = const $ H.div [ initializer ] []

  initializer :: Prop AceInput
  initializer = H.Initializer ("ace-" ++ key ++ "-init") (\el -> action (Init el))

  eval :: Eval AceInput AceState AceInput (Aff (AceEffects eff))
  eval (Init el next) = do
    editor <- liftEff' $ Ace.editNode el Ace.ace
    modify _ { editor = Just editor }
    session <- liftEff' $ Editor.getSession editor
    subscribe $ eventSource_ (Session.onChange session) $ do
      text <- liftEff $ Editor.getValue editor
      pure $ action (ChangeText text)
    pure next
  eval (ChangeText text next) = do
    liftEff' $ log text
    state <- gets (_.editor)
    case state of
      Nothing -> pure next
      Just editor -> do
        current <- liftEff' $ Editor.getValue editor
        when (text /= current) $ void $ liftEff' $ Editor.setValue text Nothing editor
        pure next
    pure next
