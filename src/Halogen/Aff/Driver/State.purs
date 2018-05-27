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

import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Component (Component')
import Halogen.Data.OrdBox (OrdBox)
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
-- | - `eff` is theect row for the target `Aff`
newtype DriverState h r s f z g p i o = DriverState (DriverStateRec h r s f z g p i o)

type DriverStateRec h r s f z g p i o =
  { component :: Component' h s z g p i o Aff
  , state :: s
  , refs :: M.Map String Element
  , children :: M.Map (OrdBox p) (Ref (DriverStateX h r g))
  , childrenIn :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g)))
  , childrenOut :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g)))
  , selfRef :: Ref (DriverState h r s f z g p i o)
  , handler :: o -> Aff Unit
  , pendingQueries :: Ref (Maybe (List (Aff Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff Unit)))
  , pendingHandlers :: Ref (Maybe (List (Aff Unit)))
  , rendering :: Maybe (r s z g p o)
  , prjQuery :: forall x. f x -> Maybe (z x)
  , fresh :: Ref Int
  , subscriptions :: Ref (Maybe (M.Map Int (Aff Unit)))
  , lifecycleHandlers :: Ref LifecycleHandlers
  }

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: Type -> Type -> Type)
  (r :: Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type)
  (f :: Type -> Type)

mkDriverStateXRef
  :: forall h r s f z g p i o
   . Ref (DriverState h r s f z g p i o)
  -> Ref (DriverStateX h r f)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r f x
   . (forall s z g p i o. DriverStateRec h r s f z g p i o -> x)
  -> DriverStateX h r f
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX
  (r :: Type -> (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type)

mkRenderStateX
  :: forall r s f z g p o m
   . (forall x. f x -> Maybe (z x))
  -> m (r s z g p o)
  -> m (RenderStateX r)
mkRenderStateX _ = unsafeCoerce

unRenderStateX
  :: forall r x
   . (forall z s g p o. r s z g p o -> x)
  -> RenderStateX r
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall m h r f
   . Functor m
  => (forall z s g p o. Maybe (r s z g p o) -> m (r s z g p o))
  -> DriverStateX h r f
  -> m (RenderStateX r)
renderStateX f = unDriverStateX \st ->
  mkRenderStateX st.prjQuery (f st.rendering)

renderStateX_
  :: forall m h r f
   . Applicative m
  => (forall z s g p o. r s z g p o -> m Unit)
  -> DriverStateX h r f
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r s f z g p i o
   . Component' h s z g p i o Aff
  -> i
  -> (o -> Aff Unit)
  -> (forall x. f x -> Maybe (z x))
  -> Ref LifecycleHandlers
  -> Effect (Ref (DriverStateX h r f))
initDriverState component input handler prjQuery lchs = do
  selfRef <- Ref.new (unsafeCoerce {})
  childrenIn <- Ref.new M.empty
  childrenOut <- Ref.new M.empty
  pendingQueries <- Ref.new (component.initializer $> Nil)
  pendingOuts <- Ref.new (Just Nil)
  pendingHandlers <- Ref.new Nothing
  fresh <- Ref.new 0
  subscriptions <- Ref.new (Just M.empty)
  let
    ds :: DriverStateRec h r s f z g p i o
    ds =
      { component
      , state: component.initialState input
      , refs: M.empty
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
  Ref.write (DriverState ds) selfRef
  pure $ mkDriverStateXRef selfRef
