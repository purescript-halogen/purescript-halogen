module Halogen
  ( Driver()
  , runUI
  , actionF
  , actionCF
  , actionFC
  , requestF
  , requestCF
  , requestFC
  , module Halogen.Component
  , module Halogen.Effects
  ) where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Free
import Control.Monad.State (runState)
import Control.Monad.State.Trans (runStateT)

import Data.Coyoneda (Natural(), Coyoneda(), liftCoyoneda)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Inject (Inject, inj)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen.Component (Component(), renderComponent, queryComponent)
import Data.Functor.Coproduct (Coproduct(), coproduct)
import Halogen.Effects (HalogenEffects())
import Halogen.HTML.Renderer.VirtualDOM (renderHTML)
import Halogen.Internal.VirtualDOM (VTree(), createElement, diff, patch)
import Halogen.Query.StateF (StateF(), stateN)

type DriverState s =
  { node :: HTMLElement
  , vtree :: VTree
  , state :: s
  }

type Driver f eff = forall i. f i -> Aff (HalogenEffects eff) i

runUI :: forall eff s f. Component s f (Aff (HalogenEffects eff)) Void
      -> s
      -> Aff (HalogenEffects eff) { node :: HTMLElement, driver :: Driver f eff }
runUI c s = case renderComponent c s of
    Tuple html s' -> do
      ref <- makeVar
      let vtree = renderHTML (driver ref) html
          node = createElement vtree
      putVar ref { node: node, vtree: vtree, state: s' }
      pure { node: node, driver: driver ref }

  where

  driver :: AVar (DriverState s) -> Driver f eff
  driver ref q = runFreeM (eval ref) (queryComponent c q)

  eval :: AVar (DriverState s)
       -> Natural (Coproduct (StateF s) (Aff (HalogenEffects eff)))
                  (Aff (HalogenEffects eff))
  eval ref = coproduct runStateStep id
    where
    runStateStep :: Natural (StateF s) (Aff (HalogenEffects eff))
    runStateStep i = do
      { node: node, vtree: prev, state: s } <- takeVar ref
      case runState (stateN i) s of
        Tuple i' s' ->
          case renderComponent c s' of
            Tuple html s'' -> do
              let next = renderHTML (driver ref) html
              node' <- liftEff $ patch (diff prev next) node
              putVar ref { node: node', vtree: next, state: s'' }
              pure i'

-- | Lifts an "action" from `f Unit` into the query algebra `Free g Unit`.
-- |
-- | An action only causes effects and has no result value - for example,
-- | event handlers within the HTML will raise actions.
-- |
-- | Commonly `g` and `f` may be the same `Functor`, but when using `Coproduct`
-- | to combine multiple algebras this function performs the work of generating
-- | the correct value for the composite algebra.
actionF :: forall f g. (Functor f, Functor g, Inject f g) => (forall i. i -> f i) -> Free g Unit
actionF f = liftFI (f unit)

-- | A version of `actionF` that lifts values into `Free g Unit` via `Coyoneda`.
-- | This is useful in cases where `g` is a `Coproduct` containing `Coyoneda f`.
actionCF :: forall f g. (Functor g, Inject (Coyoneda f) g) => (forall i. i -> f i) -> Free g Unit
actionCF f = actionF (liftCoyoneda <<< f)

-- | A version of `actionF` that lifts values into `FreeC g Unit`.
-- | This is useful in cases where `g` is a `Coproduct` and none of its
-- | components have `Functor` instances - wrapping the entire `Coproduct` in
-- | `Coyoneda` give us the `Functor` instance for free.
actionFC :: forall f g. (Inject f g) => (forall i. i -> f i) -> FreeC g Unit
actionFC f = liftFCI (f unit)

-- | Lifts an "request" from `f a` into the query algebra `Free g a`.
-- |
-- | A request can cause effects as well as fetching some information from a
-- | component.
-- |
-- | Commonly `g` and `f` may be the same `Functor`, but when using `Coproduct`
-- | to combine multiple algebras this function performs the work of generating
-- | the correct value for the composite algebra.
requestF :: forall f g a. (Functor f, Functor g, Inject f g) => (forall i. (a -> i) -> f i) -> Free g a
requestF f = liftFI (f id)

-- | A version of `requestF` that lifts values into `Free g a` via `Coyoneda`.
-- | This is useful in cases where `g` is a `Coproduct` containing `Coyoneda f`.
requestCF :: forall f g a. (Functor g, Inject (Coyoneda f) g) => (forall i. (a -> i) -> f i) -> Free g a
requestCF f = requestF (liftCoyoneda <<< f)

-- | A version of `requestF` that lifts values into `FreeC g a`.
-- | This is useful in cases where `g` is a `Coproduct` and none of its
-- | components have `Functor` instances - wrapping the entire `Coproduct` in
-- | `Coyoneda` give us the `Functor` instance for free.
requestFC :: forall f g a. (Inject f g) => (forall i. (a -> i) -> f i) -> FreeC g a
requestFC f = liftFCI (f id)
