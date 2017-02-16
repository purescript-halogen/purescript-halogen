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

type LifecycleHandlers m =
  { initializers :: List (m Unit)
  , finalizers :: List (m Unit)
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
newtype DriverState h r s f z g p i o m = DriverState (DriverStateRec h r s f z g p i o m)

type DriverStateRec h r s f z g p i o m =
  { component :: Component' h s z g p i o m
  , state :: s
  , refs :: SM.StrMap Foreign
  , children :: M.Map (OrdBox p) (Ref (DriverStateX h r g m))
  , childrenIn :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g m)))
  , childrenOut :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g m)))
  , selfRef :: Ref (DriverState h r s f z g p i o m)
  , handler :: o -> m Unit
  , pendingQueries :: Ref (Maybe (List (m Unit)))
  , pendingOuts :: Ref (Maybe (List (m Unit)))
  , rendering :: Maybe (r s z g p o m)
  , prjQuery :: forall x. f x -> Maybe (z x)
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map Int (m Unit)))
  , lifecycleHandlers :: Ref (LifecycleHandlers m)
  }

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: * -> * -> *)
  (r :: * -> (* -> *) -> (* -> *) -> * -> * -> (* -> *) -> *)
  (f :: * -> *)
  (m :: * -> *)

mkDriverStateXRef
  :: forall h r s f z g p i o m
   . Ref (DriverState h r s f z g p i o m)
  -> Ref (DriverStateX h r f m)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r f m x
   . (forall s z g p i o. DriverStateRec h r s f z g p i o m -> x)
  -> DriverStateX h r f m
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX
  (r :: * -> (* -> *) -> (* -> *) -> * -> * -> (* -> *) -> *)
  (m :: * -> *)

mkRenderStateX
  :: forall r s f z g p o m y
   . (forall x. f x -> Maybe (z x))
  -> y (r s z g p o m)
  -> y (RenderStateX r m)
mkRenderStateX _ = unsafeCoerce

unRenderStateX
  :: forall r m x
   . (forall z s g p o. r s z g p o m -> x)
  -> RenderStateX r m
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall y h r f m
   . Functor y
  => (forall z s g p o. Maybe (r s z g p o m) -> y (r s z g p o m))
  -> DriverStateX h r f m
  -> y (RenderStateX r m)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX st.prjQuery (f st.rendering)

renderStateX_
  :: forall y h r f m
   . Applicative y
  => (forall z s g p o. r s z g p o m -> y Unit)
  -> DriverStateX h r f m
  -> y Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r s f z g p i o m eff
   . Component' h s z g p i o m
  -> i
  -> (o -> m Unit)
  -> (forall x. f x -> Maybe (z x))
  -> Ref (LifecycleHandlers m)
  -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f m))
initDriverState component input handler prjQuery lchs = do
  selfRef <- newRef (unsafeCoerce {})
  childrenIn <- newRef M.empty
  childrenOut <- newRef M.empty
  pendingQueries <- newRef (component.initializer $> Nil)
  pendingOuts <- newRef (Just Nil)
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
      , rendering: Nothing
      , prjQuery
      , fresh
      , subscriptions
      , lifecycleHandlers: lchs
      }
  writeRef selfRef (DriverState ds)
  pure $ mkDriverStateXRef selfRef
