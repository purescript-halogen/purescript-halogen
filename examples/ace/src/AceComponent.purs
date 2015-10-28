module AceComponent where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff.Class (liftEff)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement())

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P

import Ace.Types (ACE(), Editor())
import qualified Ace.Editor as Editor
import qualified Ace.EditSession as Session

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type AceState = { editor :: Maybe Editor }

initAceState :: AceState
initAceState = { editor: Nothing }

-- | A basic query algebra for the Ace component.
data AceQuery a
  = Init HTMLElement a
  | ChangeText String a

-- | Effects embedding the Ace editor requires.
type AceEffects eff = (ace :: ACE, avar :: AVAR | eff)

-- | The Ace component definition. We accept a key here to give each instance
-- | of the ace component a unique initializer key - recycling an initializer
-- | may result in unexpected behaviours.
ace :: forall eff. Component AceState AceQuery (Aff (AceEffects eff))
ace = component render eval
  where

  -- As we're embedding a 3rd party component we only need to create a
  -- placeholder div here and attach the initializer property.
  render :: AceState -> ComponentHTML AceQuery
  render = const $ H.div [ P.initializer \el -> action (Init el) ] []

  -- The query algebra for the component handles the initialization of the Ace
  -- editor as well as responding to the `ChangeText` action that allows us to
  -- alter the editor's state.
  eval :: Natural AceQuery (ComponentDSL AceState AceQuery (Aff (AceEffects eff)))
  eval (Init el next) = do
    editor <- liftEff' $ Ace.editNode el Ace.ace
    modify _ { editor = Just editor }
    session <- liftEff' $ Editor.getSession editor
    subscribe $ eventSource_ (Session.onChange session) do
      text <- Editor.getValue editor
      pure $ action (ChangeText text)
    pure next
  eval (ChangeText text next) = do
    state <- gets _.editor
    case state of
      Nothing -> pure unit
      Just editor -> do
        current <- liftEff' $ Editor.getValue editor
        when (text /= current) $ void $ liftEff' $ Editor.setValue text Nothing editor
    pure next
