module Halogen.Aff.Driver.State
  ( LifecycleHandlers
  , DriverState(..)
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
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Component (ComponentSpec)
import Halogen.Data.Slot (SlotStorage)
import Halogen.Data.Slot as SlotStorage
import Halogen.Query.EventSource (Finalizer)
import Halogen.Query.HalogenM (SubscriptionId)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

type LifecycleHandlers =
  { initializers :: List (Aff Unit)
  , finalizers :: List (Aff Unit)
  }

-- | The type used to track a driver's persistent state.
-- |
-- | - `h` is the type of value the components produce for rendering.
-- | - `r` is the type for the render state for the driver.
-- | - `s` is the component state type.
-- | - `f` is the projected component query algebra - used for multi-child-type
-- |       components, by projecting to `z` we can avoid the need to remap the
-- |       entire component.
-- | - `z` is the unprojected component query algebra.
-- | - `g` is the component child query algebra.
-- | - `p` is the type of slots for the component.
-- | - `i` is the invput value type.
-- | - `o` is the type of output messages from the component.
newtype DriverState h r s f ps i o = DriverState (DriverStateRec h r s f ps i o)

type DriverStateRec h r s f ps i o =
  { component :: ComponentSpec h s f ps i o Aff
  , state :: s
  , refs :: M.Map String Element
  , children :: SlotStorage ps (DriverStateRef h r)
  , childrenIn :: Ref (SlotStorage ps (DriverStateRef h r))
  , childrenOut :: Ref (SlotStorage ps (DriverStateRef h r))
  , selfRef :: Ref (DriverState h r s f ps i o)
  , handlerRef :: Ref (o -> Aff Unit)
  , pendingQueries :: Ref (Maybe (List (Aff Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff Unit)))
  , pendingHandlers :: Ref (Maybe (List (Aff Unit)))
  , rendering :: Maybe (r s f ps o)
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map SubscriptionId (Finalizer Aff)))
  , lifecycleHandlers :: Ref LifecycleHandlers
  }

newtype DriverStateRef h r f o = DriverStateRef (Ref (DriverStateX h r f o))

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: Type -> Type -> Type)
  (r :: Type -> (Type -> Type) -> # Type -> Type -> Type)
  (f :: Type -> Type)
  (o :: Type)

mkDriverStateXRef
  :: forall h r s f ps i o
   . Ref (DriverState h r s f ps i o)
  -> Ref (DriverStateX h r f o)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r f o x
   . (forall s ps i. DriverStateRec h r s f ps i o -> x)
  -> DriverStateX h r f o
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX
  (r :: Type -> (Type -> Type) -> # Type -> Type -> Type)

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
  :: forall m h r f o
   . Functor m
  => (forall s ps. Maybe (r s f ps o) -> m (r s f ps o))
  -> DriverStateX h r f o
  -> m (RenderStateX r)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX (f st.rendering)

renderStateX_
  :: forall m h r f o
   . Applicative m
  => (forall s ps. r s f ps o -> m Unit)
  -> DriverStateX h r f o
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r s f ps i o
   . ComponentSpec h s f ps i o Aff
  -> i
  -> (o -> Aff Unit)
  -> Ref LifecycleHandlers
  -> Effect (Ref (DriverStateX h r f o))
initDriverState component input handler lchs = do
  selfRef <- Ref.new (unsafeCoerce {})
  childrenIn <- Ref.new SlotStorage.empty
  childrenOut <- Ref.new SlotStorage.empty
  handlerRef <- Ref.new handler
  pendingQueries <- Ref.new (component.initializer $> Nil)
  pendingOuts <- Ref.new (Just Nil)
  pendingHandlers <- Ref.new Nothing
  fresh <- Ref.new 1
  subscriptions <- Ref.new (Just M.empty)
  let
    ds :: DriverStateRec h r s f ps i o
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
      , lifecycleHandlers: lchs
      }
  Ref.write (DriverState ds) selfRef
  pure $ mkDriverStateXRef selfRef
