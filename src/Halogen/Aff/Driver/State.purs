module Halogen.Aff.Driver.State
  ( LifecycleHandlers
  , DriverState(..)
  , mapDriverState
  , DriverStateRef(..)
  , DriverStateRec
  , DriverStateX
  , unDriverStateX
  , mkDriverStateXRef
  , RenderStateX
  , renderStateX
  , renderStateX_
  , unRenderStateX
  , initDriverState
  ) where

import Prelude

import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Fiber)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Component (ComponentSpec)
import Halogen.Data.Slot (SlotStorage)
import Halogen.Data.Slot as SlotStorage
import Halogen.Query.HalogenM (ForkId, SubscriptionId)
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

type LifecycleHandlers =
  { initializers :: List (Aff Unit)
  , finalizers :: List (Aff Unit)
  }

newtype DriverState r s f act ps i o = DriverState (DriverStateRec r s f act ps i o)

type DriverStateRec r s f act ps i o =
  { component :: ComponentSpec s f act ps i o Aff
  , state :: s
  , refs :: M.Map String Element
  , children :: SlotStorage ps (DriverStateRef r)
  , childrenIn :: Ref (SlotStorage ps (DriverStateRef r))
  , childrenOut :: Ref (SlotStorage ps (DriverStateRef r))
  , selfRef :: Ref (DriverState r s f act ps i o)
  , handlerRef :: Ref (o -> Aff Unit)
  , pendingQueries :: Ref (Maybe (List (Aff Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff Unit)))
  , pendingHandlers :: Ref (Maybe (List (Aff Unit)))
  , rendering :: Maybe (r s act ps o)
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map SubscriptionId HS.Subscription))
  , forks :: Ref (M.Map ForkId (Fiber Unit))
  , lifecycleHandlers :: Ref LifecycleHandlers
  }

mapDriverState
  :: forall r s f act ps i o
   . (DriverStateRec r s f act ps i o -> DriverStateRec r s f act ps i o)
  -> DriverState r s f act ps i o
  -> DriverState r s f act ps i o
mapDriverState f (DriverState ds) = DriverState (f ds)

newtype DriverStateRef r f o = DriverStateRef (Ref (DriverStateX r f o))

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (r :: Type -> Type -> Row Type -> Type -> Type)
  (f :: Type -> Type)
  (o :: Type)

mkDriverStateXRef
  :: forall r s f act ps i o
   . Ref (DriverState r s f act ps i o)
  -> Ref (DriverStateX r f o)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall r f i o x
   . (forall s act ps. DriverStateRec r s f act ps i o -> x)
  -> DriverStateX r f o
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX (r :: Type -> Type -> Row Type -> Type -> Type)

mkRenderStateX
  :: forall r s f ps o m
   . m (r s f ps o)
  -> m (RenderStateX r)
mkRenderStateX = unsafeCoerce

unRenderStateX
  :: forall r x
   . (forall s f ps o. r s f ps o -> x)
  -> RenderStateX r
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall m r f o
   . Functor m
  => (forall s act ps. Maybe (r s act ps o) -> m (r s act ps o))
  -> DriverStateX r f o
  -> m (RenderStateX r)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX (f st.rendering)

renderStateX_
  :: forall m r f o
   . Applicative m
  => (forall s act ps. r s act ps o -> m Unit)
  -> DriverStateX r f o
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall r s f act ps i o
   . ComponentSpec s f act ps i o Aff
  -> i
  -> (o -> Aff Unit)
  -> Ref LifecycleHandlers
  -> Effect (Ref (DriverStateX r f o))
initDriverState component input handler lchs = do
  selfRef <- Ref.new (unsafeCoerce {})
  childrenIn <- Ref.new SlotStorage.empty
  childrenOut <- Ref.new SlotStorage.empty
  handlerRef <- Ref.new handler
  pendingQueries <- Ref.new (Just Nil)
  pendingOuts <- Ref.new (Just Nil)
  pendingHandlers <- Ref.new Nothing
  fresh <- Ref.new 1
  subscriptions <- Ref.new (Just M.empty)
  forks <- Ref.new M.empty
  let
    ds :: DriverStateRec r s f act ps i o
    ds =
      { component
      , state: component.initialState input
      , refs: M.empty
      , children: SlotStorage.empty
      , childrenIn
      , childrenOut
      , selfRef
      , handlerRef
      , pendingQueries
      , pendingOuts
      , pendingHandlers
      , rendering: Nothing
      , fresh
      , subscriptions
      , forks
      , lifecycleHandlers: lchs
      }
  Ref.write (DriverState ds) selfRef
  pure $ mkDriverStateXRef selfRef
