module Halogen.Aff.Driver.State
  ( LifecycleHandlers
  , DriverState(..)
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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef)

import Data.Foreign (Foreign)
import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.StrMap as SM
import Data.Traversable (traverse_)

import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component')
import Halogen.Data.OrdBox (OrdBox)

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
newtype DriverState h r s f z g p i o eff = DriverState (DriverStateRec h r s f z g p i o eff)

type DriverStateRec h r s f z g p i o eff =
  { component :: Component' h s z g p i o (Aff (HalogenEffects eff))
  , state :: s
  , refs :: SM.StrMap Foreign
  , children :: M.Map (OrdBox p) (Ref (DriverStateX h r g eff))
  , childrenIn :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
  , childrenOut :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
  , selfRef :: Ref (DriverState h r s f z g p i o eff)
  , handler :: o -> Aff (HalogenEffects eff) Unit
  , pendingQueries :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , pendingHandlers :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , rendering :: Maybe (r s z g p o eff)
  , prjQuery :: forall x. f x -> Maybe (z x)
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map Int (Aff (HalogenEffects eff) Unit)))
  , lifecycleHandlers :: Ref (LifecycleHandlers eff)
  }

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: * -> * -> *)
  (r :: * -> (* -> *) -> (* -> *) -> * -> * -> # ! -> *)
  (f :: * -> *)
  (eff :: # !)

mkDriverStateXRef
  :: forall h r s f z g p i o eff
   . Ref (DriverState h r s f z g p i o eff)
  -> Ref (DriverStateX h r f eff)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r f eff x
   . (forall s z g p i o. DriverStateRec h r s f z g p i o eff -> x)
  -> DriverStateX h r f eff
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX
  (r :: * -> (* -> *) -> (* -> *) -> * -> * -> # ! -> *)
  (eff :: # !)

mkRenderStateX
  :: forall r s f z g p o eff m
   . (forall x. f x -> Maybe (z x))
  -> m (r s z g p o eff)
  -> m (RenderStateX r eff)
mkRenderStateX _ = unsafeCoerce

unRenderStateX
  :: forall r eff x
   . (forall z s g p o. r s z g p o eff -> x)
  -> RenderStateX r eff
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall m h r f eff
   . Functor m
  => (forall z s g p o. Maybe (r s z g p o eff) -> m (r s z g p o eff))
  -> DriverStateX h r f eff
  -> m (RenderStateX r eff)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX st.prjQuery (f st.rendering)

renderStateX_
  :: forall m h r f eff
   . Applicative m
  => (forall z s g p o. r s z g p o eff -> m Unit)
  -> DriverStateX h r f eff
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r s f z g p i o eff
   . Component' h s z g p i o (Aff (HalogenEffects eff))
  -> i
  -> (o -> Aff (HalogenEffects eff) Unit)
  -> (forall x. f x -> Maybe (z x))
  -> Ref (LifecycleHandlers eff)
  -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f eff))
initDriverState component input handler prjQuery lchs = do
  selfRef <- newRef (unsafeCoerce {})
  childrenIn <- newRef M.empty
  childrenOut <- newRef M.empty
  pendingQueries <- newRef (component.initializer $> Nil)
  pendingOuts <- newRef (Just Nil)
  pendingHandlers <- newRef Nothing
  fresh <- newRef 0
  subscriptions <- newRef (Just M.empty)
  let
    ds =
      { component
      , state: component.initialState input
      , refs: SM.empty
      , children: M.empty
      , childrenIn
      , childrenOut
      , selfRef
      , handler
      , pendingQueries
      , pendingOuts
      , pendingHandlers
      , rendering: Nothing
      , prjQuery
      , fresh
      , subscriptions
      , lifecycleHandlers: lchs
      }
  writeRef selfRef (DriverState ds)
  pure $ mkDriverStateXRef selfRef
