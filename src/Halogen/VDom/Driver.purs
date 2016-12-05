module Halogen.VDom.Driver
  ( runUI
  , module Halogen
  , module Halogen.Effects
  )
  where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Parallel (parSequence_)

import Data.Either (Either(..))
import Data.List ((:))
import Data.List as L
import Data.Lazy
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple

import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.HTML.Window (document) as DOM
import DOM.Node.Node (appendChild)
import DOM.Node.Types (Document, Element, Node) as DOM

import Halogen (HalogenIO)
import Halogen.Data.OrdBox
import Halogen.Aff.Driver.Eval (LifecycleHandlers, eval, handleLifecycle)
import Halogen.Aff.Driver.State (ComponentType(..), DriverStateX, DriverState(..), unDriverStateX, initDriverState)
import Halogen.Component
import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core (HTML, Prop)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as VP

type VHTML f g p eff =
  V.VDom (Array (Prop (f Unit))) (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))

newtype RenderState s f g p o eff =
  RenderState (V.Step (Eff (HalogenEffects eff)) (VHTML f g p eff) DOM.Node)

mkSpec
  :: forall s f g p o eff
   . Ref (DriverState HTML RenderState s f g p o eff)
  -> (f Unit -> Eff (HalogenEffects eff) Unit)
  -> DOM.Document
  -> V.VDomSpec
      (HalogenEffects eff)
      (Array (VP.Prop (f Unit)))
      (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
mkSpec ref handler document = V.VDomSpec
  { buildWidget: buildWidget
  , buildAttributes: buildAttributes
  , document: document
  }
  where

  buildAttributes
    :: DOM.Element
    -> V.VDomMachine (HalogenEffects eff) (Array (VP.Prop (f Unit))) Unit
  buildAttributes = VP.buildProp handler

  buildWidget
    :: V.VDomSpec (HalogenEffects eff)
          (Array (VP.Prop (f Unit)))
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
    -> V.VDomMachine (HalogenEffects eff)
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
          DOM.Node
  buildWidget spec slot = do
    -- DriverState ds <- readRef ref
    -- case M.lookup ds.children of
    --   Nothing
    machine <- V.buildVDom spec (V.Text "<machine>")
    let node = V.extract machine
    pure (V.Step node (patch node) done)

  patch
    :: DOM.Node
    -> V.VDomMachine (HalogenEffects eff)
          (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit))
          DOM.Node
  patch node x = pure (V.Step node (patch node) done)

  done :: Eff (HalogenEffects eff) Unit
  done = pure unit

runUI
  :: forall f o eff
   . Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI component container = do
  document <- DOM.htmlDocumentToDocument <$> liftEff (DOM.document =<< DOM.window)
  runUI' document component container

runUI'
  :: forall f o eff
   . DOM.Document
  -> Component HTML f o (Aff (HalogenEffects eff))
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI' document component container = do
  fresh <- liftEff $ newRef 0
  handleLifecycle \lchs -> liftEff $ do
    listeners <- newRef M.empty
    runComponent (rootHandler listeners) fresh lchs Root component
      >>= readRef
      >>= unDriverStateX \st ->
        pure
          { query: evalF st.selfRef
          , subscribe: subscribe fresh listeners
          }

  where

  evalF
    :: forall s f' g p o'
     . Ref (DriverState HTML RenderState s f' g p o' eff)
    -> f'
    ~> Aff (HalogenEffects eff)
  evalF ref = eval render ref

  rootHandler
    :: Ref (M.Map Int (AV.AVar o))
    -> o
    -> Aff (HalogenEffects eff) Unit
  rootHandler ref message = do
    listeners <- liftEff $ readRef ref
    void $ forkAll $ map (\var -> AV.putVar var message) listeners

  subscribe
    :: Ref Int
    -> Ref (M.Map Int (AV.AVar o))
    -> CR.Consumer o (Aff (HalogenEffects eff)) Unit
    -> Aff (HalogenEffects eff) Unit
  subscribe fresh ref consumer = do
    inputVar <- AV.makeVar
    listenerId <- liftEff do
      listenerId <- readRef fresh
      modifyRef fresh (_ + 1)
      modifyRef ref (M.insert listenerId inputVar)
      pure listenerId
    let producer = CR.producer (Left <$> AV.peekVar inputVar)
    void $ forkAff do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      AV.killVar inputVar (error "ended")

  runComponent
    :: forall f' o'
     . (o' -> Aff (HalogenEffects eff) Unit)
    -> Ref Int
    -> Ref (LifecycleHandlers eff)
    -> ComponentType
    -> Component HTML f' o' (Aff (HalogenEffects eff))
    -> Eff (HalogenEffects eff) (Ref (DriverStateX HTML RenderState f' eff))
  runComponent handler fresh lchs componentType = unComponent \c -> do
    keyId <- readRef fresh
    modifyRef fresh (_ + 1)
    var <- initDriverState c componentType handler keyId fresh
    unDriverStateX (render lchs <<< _.selfRef) =<< readRef var
    squashChildInitializers lchs =<< readRef var
    pure var

  render
    :: forall s f' g p o'
     . Ref (LifecycleHandlers eff)
    -> Ref (DriverState HTML RenderState s f' g p o' eff)
    -> Eff (HalogenEffects eff) Unit
  render lchs var = readRef var >>= \(DriverState ds) -> do
    childrenVar <- newRef M.empty
    oldChildren <- newRef ds.children
    let
      selfEval = evalF ds.selfRef
      handler :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler = void <<< selfEval
      handler' :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler' = maybe handler (\_ -> queuingHandler ds.selfRef handler) ds.pendingIn
      vdom = unwrap (ds.component.render ds.state)
    rendering <- liftEff case ds.rendering of
      Nothing -> do
        let spec = mkSpec ds.selfRef (handleAff <<< selfEval) document
        machine <- V.buildVDom spec vdom
        appendChild (V.extract machine) (htmlElementToNode container)
        pure $ RenderState machine
      Just (RenderState machine) ->
        RenderState <$> V.step machine vdom
      -- renderSpec.render
      --   (handleAff <<< selfEval)
      --   (renderChild handler' ds.fresh ds.mkOrdBox oldChildren childrenVar lchs)
      --   (ds.component.render ds.state)
      --   ds.componentType
      --   ds.rendering
    children <- readRef childrenVar
    traverse_ (addFinalizer lchs <=< readRef) =<< readRef oldChildren
    writeRef var $
      DriverState
        { rendering: Just rendering
        , componentType: ds.componentType
        , component: ds.component
        , state: ds.state
        , children
        , mkOrdBox: ds.mkOrdBox
        , selfRef: ds.selfRef
        , handler: ds.handler
        , pendingIn: ds.pendingIn
        , pendingOut: ds.pendingOut
        , keyId: ds.keyId
        , fresh: ds.fresh
        }

  queuingHandler
    :: forall s f' g p o' x
     . Ref (DriverState HTML RenderState s f' g p o' eff)
    -> (f' x -> Aff (HalogenEffects eff) Unit)
    -> f' x
    -> Aff (HalogenEffects eff) Unit
  queuingHandler var handler message = do
    DriverState (ds@{ pendingIn }) <- liftEff (readRef var)
    case pendingIn of
      Nothing -> do
        liftEff $ writeRef var (DriverState ds)
        handler message
      Just p ->
        liftEff $ writeRef var (DriverState ds { pendingIn = Just (handler message : p) })

  renderChild
    :: forall f' g p
     . (forall x. f' x -> Aff (HalogenEffects eff) Unit)
    -> Ref Int
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX HTML RenderState g eff)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX HTML RenderState g eff)))
    -> Ref (LifecycleHandlers eff)
    -> ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f' Unit)
    -> Eff (HalogenEffects eff) Unit
  renderChild handler fresh mkOrdBox childrenInRef childrenOutRef lchs =
    unComponentSlot \p ctor k -> do
      childrenIn <- readRef childrenInRef
      var <- case M.pop (mkOrdBox p) childrenIn of
        Just (Tuple existing childrenIn') -> do
          writeRef childrenInRef childrenIn'
          pure existing
        Nothing ->
          runComponent (maybe (pure unit) handler <<< k) fresh lchs Child (force ctor)
      modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
      -- unDriverStateX (\st -> ?renderChild st.keyId st.rendering) =<< readRef var

  squashChildInitializers
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX HTML RenderState f' eff
    -> Eff (HalogenEffects eff) Unit
  squashChildInitializers ref =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <$> st.component.initializer
      modifyRef ref \lchs ->
        { initializers: pure $ do
            parSequence_ (L.reverse lchs.initializers)
            sequence_ parentInitializer
            handlePending st.selfRef
        , finalizers: lchs.finalizers
        }

  handlePending
    :: forall s f' g p o'
     . Ref (DriverState HTML RenderState s f' g p o' eff)
    -> Aff (HalogenEffects eff) Unit
  handlePending ref = do
    DriverState (dsi@{ pendingIn }) <- liftEff (readRef ref)
    liftEff $ writeRef ref (DriverState dsi { pendingIn = Nothing })
    for_ pendingIn (forkAll <<< L.reverse)
    DriverState (dso@{ pendingOut, handler }) <- liftEff (readRef ref)
    liftEff $ writeRef ref (DriverState dso { pendingOut = Nothing })
    for_ pendingOut (forkAll <<< map handler <<< L.reverse)

  addFinalizer
    :: forall f'
     . Ref (LifecycleHandlers eff)
    -> DriverStateX HTML RenderState f' eff
    -> Eff (HalogenEffects eff) Unit
  addFinalizer ref =
    unDriverStateX \st -> do
      for_ (evalF st.selfRef <$> st.component.finalizer) \f ->
        modifyRef ref (\lchs ->
          { initializers: lchs.initializers
          , finalizers: f : lchs.finalizers
          })
      for_ st.children (addFinalizer ref <=< readRef)

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall eff a
   . Aff (HalogenEffects eff) a
  -> Eff (HalogenEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))
