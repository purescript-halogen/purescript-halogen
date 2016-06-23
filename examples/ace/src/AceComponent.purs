module AceComponent where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Ace as Ace
import Ace.Editor as Editor
import Ace.EditSession as Session
import Ace.Types (ACE, Editor)

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
ace :: forall eff. H.Component AceState AceQuery (Aff (AceEffects eff))
ace = H.lifecycleComponent
    { render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    }
  where

  -- As we're embedding a 3rd party component we only need to create a
  -- placeholder div here and attach the ref property which will raise a query
  -- when the element is created.
  render :: AceState -> H.ComponentHTML AceQuery
  render = const $ HH.div [ HP.ref \el -> H.action (SetElement el) ] []

  -- The query algebra for the component handles the initialization of the Ace
  -- editor as well as responding to the `ChangeText` action that allows us to
  -- alter the editor's state.
  eval :: AceQuery ~> H.ComponentDSL AceState AceQuery (Aff (AceEffects eff))
  eval (SetElement el next) = do
    H.modify (_ { element = el })
    pure next
  eval (Initialize next) = do
    el <- H.gets _.element
    case el of
      Nothing -> pure unit
      Just el' -> do
        editor <- H.fromEff $ Ace.editNode el' Ace.ace
        H.modify (_ { editor = Just editor })
        session <- H.fromEff $ Editor.getSession editor
        H.subscribe $ H.eventSource_ (Session.onChange session) do
          text <- Editor.getValue editor
          pure $ H.action (ChangeText text)
    pure next
  eval (Finalize next) = do
    -- Release the reference to the editor and do any other cleanup that a real
    -- world component might need.
    H.modify (_ { editor = Nothing })
    pure next
  eval (ChangeText text next) = do
    state <- H.gets _.editor
    case state of
      Nothing -> pure unit
      Just editor -> do
        current <- H.fromEff $ Editor.getValue editor
        when (text /= current) $ void $ H.fromEff $ Editor.setValue text Nothing editor
    pure next
