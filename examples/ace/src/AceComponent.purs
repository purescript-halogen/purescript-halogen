module AceComponent (AceEffects, AceQuery(..), AceOutput(..), aceComponent) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)

import Data.Maybe (Maybe(..))
import Data.Const (Const)

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
type AceState = { editor :: Maybe Editor }

-- | A basic query algebra for the Ace component.
data AceQuery a
  = Initialize a
  | Finalize a
  | ChangeText String a
  | HandleChange (H.SubscribeStatus -> a)

data AceOutput = TextChanged String

-- | Effects embedding the Ace editor requires.
type AceEffects eff = (ace :: ACE, avar :: AVAR | eff)

-- | The Ace component definition.
aceComponent :: forall eff. H.Component HH.HTML AceQuery Unit AceOutput (Aff (AceEffects eff))
aceComponent =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Just (H.action Finalize)
    , receiver: const Nothing
    }
  where

  initialState :: AceState
  initialState =
    { editor: Nothing }

  -- As we're embedding a 3rd party component we only need to create a
  -- placeholder div here and attach the ref property which will raise a query
  -- when the element is created.
  render :: AceState -> H.ParentHTML AceQuery (Const Void) Unit (Aff (AceEffects eff))
  render = const $ HH.div [ HP.ref unit ] []

  -- The query algebra for the component handles the initialization of the Ace
  -- editor as well as responding to the `ChangeText` action that allows us to
  -- alter the editor's state.
  eval :: AceQuery ~> H.ParentDSL AceState AceQuery (Const Void) Unit AceOutput (Aff (AceEffects eff))
  eval (Initialize next) = do
    H.getHTMLElementRef unit >>= case _ of
      Nothing -> pure unit
      Just el' -> do
        editor <- H.liftEff $ Ace.editNode el' Ace.ace
        session <- H.liftEff $ Editor.getSession editor
        H.modify (_ { editor = Just editor })
        H.subscribe $ H.eventSource_ (Session.onChange session) (H.request HandleChange)
    pure next
  eval (Finalize next) = do
    -- Release the reference to the editor and do any other cleanup that a real
    -- world component might need.
    H.modify (_ { editor = Nothing })
    pure next
  eval (ChangeText text next) = do
    maybeEditor <- H.gets _.editor
    case maybeEditor of
      Nothing -> pure unit
      Just editor -> do
        current <- H.liftEff $ Editor.getValue editor
        when (text /= current) do
          void $ H.liftEff $ Editor.setValue text Nothing editor
    H.raise $ TextChanged text
    pure next
  eval (HandleChange reply) = do
    maybeEditor <- H.gets _.editor
    case maybeEditor of
      Nothing -> pure unit
      Just editor -> do
        text <- H.liftEff (Editor.getValue editor)
        H.raise $ TextChanged text
    pure (reply H.Listening)
