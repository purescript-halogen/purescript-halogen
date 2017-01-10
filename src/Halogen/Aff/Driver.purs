module Halogen.Aff.Driver
  ( RenderSpec
  , runUI
  , module Halogen
  , module Halogen.Aff.Effects
  ) where

import Prelude

import Control.Coroutine as CR
import Control.Monad.Aff (Aff, forkAff, forkAll, runAff)
import Control.Monad.Aff.AVar as AV
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error, throw, throwException)
import Control.Monad.Eff.Ref (Ref, modifyRef, writeRef, readRef, newRef)
import Control.Parallel (parSequence_)

import Data.Lazy (force)
import Data.List ((:))
import Data.List as L
import Data.Either (Either(..))
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for_, traverse_, sequence_)
import Data.Tuple (Tuple(..))

import Halogen (HalogenIO)
import Halogen.Aff.Driver.Eval (LifecycleHandlers, eval, handleLifecycle, queuingHandler)
import Halogen.Aff.Driver.State (DriverState(..), DriverStateX, RenderStateX, initDriverState, renderStateX, unDriverStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component, ComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.OrdBox (OrdBox)

type RenderSpec h r eff =
  { render
      :: forall s f g p o
       . (forall x. f x -> Eff (HalogenEffects eff) Unit)
      -> (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX h r g eff))
      -> h (ComponentSlot h g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
      -> Maybe (r s f g p o eff)
      -> Eff (HalogenEffects eff) (r s f g p o eff)
  , renderChild :: forall s f g p o. r s f g p o eff -> r s f g p o eff
  }

runUI
  :: forall h r f o eff
   . RenderSpec h r eff
  -> Component h f o (Aff (HalogenEffects eff))
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI renderSpec component = do
  lchs <- liftEff $ newRef { initializers: L.Nil, finalizers: L.Nil }
  runUI' lchs renderSpec component

runUI'
  :: forall h r f o eff
   . Ref (LifecycleHandlers eff)
  -> RenderSpec h r eff
  -> Component h f o (Aff (HalogenEffects eff))
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI' lchs renderSpec component = do
  fresh <- liftEff $ newRef 0
  handleLifecycle lchs do
    listeners <- newRef M.empty
    runComponent (rootHandler listeners) component
      >>= readRef
      >>= unDriverStateX \st ->
        pure
          { query: evalF st.selfRef
          , subscribe: subscribe fresh listeners
          }

  where

  evalF
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> f'
    ~> Aff (HalogenEffects eff)
  evalF ref = eval lchs render ref

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
    let producer = CR.producer (Left <$> AV.takeVar inputVar)
    void $ forkAff do
      CR.runProcess (CR.connect producer consumer)
      liftEff $ modifyRef ref (M.delete listenerId)
      AV.killVar inputVar (error "ended")

  runComponent
    :: forall f' o'
     . (o' -> Aff (HalogenEffects eff) Unit)
    -> Component h f' o' (Aff (HalogenEffects eff))
    -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f' eff))
  runComponent handler = unComponent \c -> do
    var <- initDriverState c handler
    unDriverStateX (render <<< _.selfRef) =<< readRef var
    squashChildInitializers =<< readRef var
    pure var

  render
    :: forall s f' g p o'
     . Ref (DriverState h r s f' g p o' eff)
    -> Eff (HalogenEffects eff) Unit
  render var = readRef var >>= \(DriverState ds) -> do
    writeRef ds.childrenOut M.empty
    writeRef ds.childrenIn ds.children
    let
      handler :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      handler = void <<< evalF ds.selfRef
      selfHandler :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      selfHandler = queuingHandler handler ds.pendingRefs
      childHandler :: forall x. f' x -> Aff (HalogenEffects eff) Unit
      childHandler = queuingHandler handler ds.pendingQueries
    rendering <-
      renderSpec.render
        (handleAff <<< selfHandler)
        (renderChild childHandler ds.mkOrdBox ds.childrenIn ds.childrenOut)
        (ds.component.render ds.state)
        ds.rendering
    children <- readRef ds.childrenOut
    traverse_ (addFinalizer <=< readRef) =<< readRef ds.childrenIn
    modifyRef var \(DriverState ds') ->
      DriverState
        { rendering: Just rendering
        , children
        , component: ds'.component
        , state: ds'.state
        , childrenIn: ds'.childrenIn
        , childrenOut: ds'.childrenOut
        , mkOrdBox: ds'.mkOrdBox
        , selfRef: ds'.selfRef
        , handler: ds'.handler
        , pendingRefs: ds'.pendingRefs
        , pendingQueries: ds'.pendingQueries
        , pendingOuts: ds'.pendingOuts
        }

  renderChild
    :: forall f' g p
     . (forall x. f' x -> Aff (HalogenEffects eff) Unit)
    -> (p -> OrdBox p)
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
    -> ComponentSlot h g (Aff (HalogenEffects eff)) p (f' Unit)
    -> Eff (HalogenEffects eff) (RenderStateX h r g eff)
  renderChild handler mkOrdBox childrenInRef childrenOutRef =
    unComponentSlot \p ctor k -> do
      childrenIn <- readRef childrenInRef
      var <- case M.pop (mkOrdBox p) childrenIn of
        Just (Tuple existing childrenIn') -> do
          writeRef childrenInRef childrenIn'
          pure existing
        Nothing ->
          runComponent (maybe (pure unit) handler <<< k) (force ctor)
      modifyRef childrenOutRef (M.insert (mkOrdBox p) var)
      readRef var >>= renderStateX case _ of
        Nothing -> throw "Halogen internal error: child was not initialized in renderChild"
        Just r -> pure (renderSpec.renderChild r)

  squashChildInitializers
    :: forall f'
     . DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  squashChildInitializers =
    unDriverStateX \st -> do
      let parentInitializer = evalF st.selfRef <$> st.component.initializer
      modifyRef lchs \handlers ->
        { initializers: pure $ do
            queue <- liftEff (readRef st.pendingRefs)
            liftEff $ writeRef st.pendingRefs Nothing
            for_ queue parSequence_
            parSequence_ (L.reverse handlers.initializers)
            sequence_ parentInitializer
            handlePending st.pendingQueries
            handlePending st.pendingOuts
        , finalizers: handlers.finalizers
        }

  handlePending
    :: Ref (Maybe (L.List (Aff (HalogenEffects eff) Unit)))
    -> Aff (HalogenEffects eff) Unit
  handlePending ref = do
    queue <- liftEff (readRef ref)
    liftEff $ writeRef ref Nothing
    for_ queue (forkAll <<< L.reverse)

  addFinalizer
    :: forall f'
     . DriverStateX h r f' eff
    -> Eff (HalogenEffects eff) Unit
  addFinalizer =
    unDriverStateX \st -> do
      for_ (evalF st.selfRef <$> st.component.finalizer) \f ->
        modifyRef lchs (\handlers ->
          { initializers: handlers.initializers
          , finalizers: f : handlers.finalizers
          })
      for_ st.children (addFinalizer <=< readRef)

-- | TODO: we could do something more intelligent now this isn't baked into the
-- | virtual-dom rendering. Perhaps write to an avar when an error occurs...
-- | something other than a runtime exception anyway.
handleAff
  :: forall eff a
   . Aff (HalogenEffects eff) a
  -> Eff (HalogenEffects eff) Unit
handleAff = void <<< runAff throwException (const (pure unit))
