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

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef)
import Data.Foreign (Foreign)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (traverse_)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component')
import Halogen.Data.Slot (SlotStorage)
import Halogen.Data.Slot as SlotStorage
import Unsafe.Coerce (unsafeCoerce)

type LifecycleHandlers eff =
  { initializers :: List (Aff (HalogenEffects eff) Unit)
  , finalizers :: List (Aff (HalogenEffects eff) Unit)
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
-- | - `eff` is the effect row for the target `Aff`
newtype DriverState h r s f ps i o eff = DriverState (DriverStateRec h r s f ps i o eff)

type DriverStateRec h r s f ps i o eff =
  { component :: Component' h s f ps i o (Aff (HalogenEffects eff))
  , state :: s
  , refs :: SM.StrMap Foreign
  , children :: SlotStorage ps (DriverStateRef h r eff)
  , childrenIn :: Ref (SlotStorage ps (DriverStateRef h r eff))
  , childrenOut :: Ref (SlotStorage ps (DriverStateRef h r eff))
  , selfRef :: Ref (DriverState h r s f ps i o eff)
  , handlerRef :: Ref (o -> Aff (HalogenEffects eff) Unit)
  , pendingQueries :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , pendingHandlers :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , rendering :: Maybe (r s f ps o eff)
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map Int (Aff (HalogenEffects eff) Unit)))
  , lifecycleHandlers :: Ref (LifecycleHandlers eff)
  }

newtype DriverStateRef h r eff f o = DriverStateRef (Ref (DriverStateX h r eff f o))

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: Type -> Type -> Type)
  (r :: Type -> (Type -> Type) -> # Type -> Type -> # Effect -> Type)
  (eff :: # Effect)
  (f :: Type -> Type)
  (o :: Type)

mkDriverStateXRef
  :: forall h r s f ps i o eff
   . Ref (DriverState h r s f ps i o eff)
  -> Ref (DriverStateX h r eff f o)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r eff f o x
   . (forall s ps i. DriverStateRec h r s f ps i o eff -> x)
  -> DriverStateX h r eff f o
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX
  (r :: Type -> (Type -> Type) -> # Type -> Type -> # Effect -> Type)
  (eff :: # Effect)

mkRenderStateX
  :: forall r s f ps o eff m
   . m (r s f ps o eff)
  -> m (RenderStateX r eff)
mkRenderStateX = unsafeCoerce

unRenderStateX
  :: forall r eff x
   . (forall s f ps o. r s f ps o eff -> x)
  -> RenderStateX r eff
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall m h r eff f o
   . Functor m
  => (forall s ps. Maybe (r s f ps o eff) -> m (r s f ps o eff))
  -> DriverStateX h r eff f o
  -> m (RenderStateX r eff)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX (f st.rendering)

renderStateX_
  :: forall m h r eff f o
   . Applicative m
  => (forall s ps. r s f ps o eff -> m Unit)
  -> DriverStateX h r eff f o
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r s f ps i o eff
   . Component' h s f ps i o (Aff (HalogenEffects eff))
  -> i
  -> (o -> Aff (HalogenEffects eff) Unit)
  -> Ref (LifecycleHandlers eff)
  -> Eff (HalogenEffects eff) (Ref (DriverStateX h r eff f o))
initDriverState component input handler lchs = do
  selfRef <- newRef (unsafeCoerce {})
  childrenIn <- newRef SlotStorage.empty
  childrenOut <- newRef SlotStorage.empty
  pendingQueries <- newRef (component.initializer $> Nil)
  pendingOuts <- newRef (Just Nil)
  pendingHandlers <- newRef Nothing
  handlerRef <- newRef handler
  fresh <- newRef 0
  subscriptions <- newRef (Just M.empty)
  let
    ds :: DriverStateRec h r s f ps i o eff
    ds =
      { component
      , state: component.initialState input
      , refs: SM.empty
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
  writeRef selfRef (DriverState ds)
  pure $ mkDriverStateXRef selfRef
