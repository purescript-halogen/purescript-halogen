module AceComponent where

import Prelude

import Control.Monad (when)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement)

import Halogen
import Halogen.HTML as H
import Halogen.HTML.Properties as P

import Ace.Types (ACE, Editor)
import Ace.Editor as Editor
import Ace.EditSession as Session

-- | The state for the ace component - we only need a reference to the editor,
-- | as Ace editor has its own internal state that we can query instead of
-- | replicating it within Halogen.
type AceState =
  { element :: Maybe HTMLElement
  , editor :: Maybe Editor
  }

initAceState :: AceState
initAceState =
  { element: Nothing
  , editor: Nothing
  }

-- | A basic query algebra for the Ace component.
data AceQuery a
  = SetElement (Maybe HTMLElement) a
  | Initialize a
  | Finalize a
  | ChangeText String a

-- | Effects embedding the Ace editor requires.
type AceEffects eff = (ace :: ACE, avar :: AVAR | eff)

-- | The Ace component definition.
ace :: forall eff. Component AceState AceQuery (Aff (AceEffects eff))
ace = lifecycleComponent
    { render
    , eval
    , initializer: Just (action Initialize)
    , finalizer: Just (action Finalize)
    }
  where

  -- As we're embedding a 3rd party component we only need to create a
  -- placeholder div here and attach the ref property which will raise a query
  -- when the element is created.
  render :: AceState -> ComponentHTML AceQuery
  render = const $ H.div [ P.ref \el -> action (SetElement el) ] []

  -- The query algebra for the component handles the initialization of the Ace
  -- editor as well as responding to the `ChangeText` action that allows us to
  -- alter the editor's state.
  eval :: Natural AceQuery (ComponentDSL AceState AceQuery (Aff (AceEffects eff)))
  eval (SetElement el next) = do
    modify (_ { element = el })
    pure next
  eval (Initialize next) = do
    el <- gets _.element
    case el of
      Nothing -> pure unit
      Just el' -> do
        editor <- fromEff $ Ace.editNode el' Ace.ace
        modify (_ { editor = Just editor })
        session <- fromEff $ Editor.getSession editor
        subscribe $ eventSource_ (Session.onChange session) do
          text <- Editor.getValue editor
          pure $ action (ChangeText text)
    pure next
  eval (Finalize next) = do
    -- Release the reference to the editor and do any other cleanup that a real
    -- world component might need.
    modify (_ { editor = Nothing })
    pure next
  eval (ChangeText text next) = do
    state <- gets _.editor
    case state of
      Nothing -> pure unit
      Just editor -> do
        current <- fromEff $ Editor.getValue editor
        when (text /= current) $ void $ fromEff $ Editor.setValue text Nothing editor
    pure next
