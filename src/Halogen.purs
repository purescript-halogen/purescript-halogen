module Halogen
  ( Driver()
  , runUI
  , actionF
  , actionFC
  , requestF
  , requestFC
  , module Halogen.Component
  , module Halogen.Effects
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff.Ref (Ref(), newRef, writeRef, readRef)
import Control.Monad.Free
import Control.Monad.State (runState)
import Control.Monad.State.Trans (runStateT)

import Data.Coyoneda (Coyoneda(), liftCoyoneda)
import Data.DOM.Simple.Types (HTMLElement())
import Data.Inject (Inject, inj)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Void (Void())

import Halogen.Component (Component(), renderComponent, queryComponent)
import Halogen.Effects (HalogenEffects())
import Halogen.HTML.Renderer.VirtualDOM (renderHTML)
import Halogen.Internal.VirtualDOM (VTree(), createElement, diff, patch)

type DriverState s =
  Maybe { node :: HTMLElement
        , vtree :: VTree
        , state :: s
        }

type Driver f eff = forall i. f i -> Eff (HalogenEffects eff) i

runUI :: forall eff s f. Component s f (Eff (HalogenEffects eff)) Void
      -> s
      -> Eff (HalogenEffects eff) { node :: HTMLElement, driver :: Driver f eff }
runUI c s = case renderComponent c s of
    Tuple html s' -> do
      ref <- newRef Nothing
      let vtree = renderHTML (driver ref) html
          node = createElement vtree
      writeRef ref $ Just { node: node, vtree: vtree, state: s' }
      pure { node: node, driver: driver ref }

  where

  driver :: Ref (DriverState s) -> Driver f eff
  driver ref q = do
    refVal <- readRef ref
    case refVal of
      Nothing -> throwException $ error "Error: An attempt to re-render was made during the initial render."
      Just { node: node, vtree: prev, state: s } -> do
        Tuple i s' <- queryComponent c q s
        case renderComponent c s' of
          Tuple html s'' -> do
            let next = renderHTML (driver ref) html
            node' <- patch (diff prev next) node
            writeRef ref $ Just { node: node', vtree: next, state: s'' }
            pure i

actionF :: forall f g. (Functor f, Functor g, Inject f g) => (forall i. i -> f i) -> Free g Unit
actionF f = liftF (inj (f unit) :: g Unit)

requestF :: forall f g a. (Functor f, Functor g, Inject f g) => (forall i. (a -> i) -> f i) -> Free g a
requestF f = liftF (inj (f id) :: g a)

actionFC :: forall f g. (Functor g, Inject (Coyoneda f) g) => (forall i. i -> f i) -> Free g Unit
actionFC f = actionF (liftCoyoneda <<< f)

requestFC :: forall f g a. (Functor g, Inject (Coyoneda f) g) => (forall i. (a -> i) -> f i) -> Free g a
requestFC f = requestF (liftCoyoneda <<< f)
