module Halogen.Aff.Driver.Implementation.Utils where

import Prelude

import Control.Monad.Fork.Class (fork)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, sequence_, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, killFiber)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver.Eval as Eval
import Halogen.Aff.Driver.State (DriverState(..), LifecycleHandlers)

-- | Functions that are not dependent on any rendering

newLifecycleHandlers :: Effect (Ref LifecycleHandlers)
newLifecycleHandlers = Ref.new { initializers: L.Nil, finalizers: L.Nil }

handlePending :: Ref (Maybe (L.List (Aff Unit))) -> Effect Unit
handlePending ref = do
  queue <- Ref.read ref
  Ref.write Nothing ref
  for_ queue (Eval.handleAff <<< traverse_ fork <<< L.reverse)

cleanupSubscriptionsAndForks
  :: forall r s f act ps i o
   . DriverState r s f act ps i o
  -> Effect Unit
cleanupSubscriptionsAndForks (DriverState ds) = do
  traverse_ sequence_ =<< Ref.read ds.subscriptions
  Ref.write Nothing ds.subscriptions
  traverse_ (Eval.handleAff <<< killFiber (error "finalized")) =<< Ref.read ds.forks
  Ref.write M.empty ds.forks
