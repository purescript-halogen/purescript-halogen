module Halogen.Aff.Driver.State
  ( DriverState(..)
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

import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)

import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (Component')
import Halogen.Data.OrdBox (OrdBox)

import Unsafe.Coerce (unsafeCoerce)

-- | The type used to track a driver's persistent state.
-- |
-- | - `h` is the type of value the components produce for rendering.
-- | - `r` is the type for the render state for the driver.
-- | - `f` is the component query algebra.
-- | - `g` is the component child query algebra.
-- | - `p` is the type of slots for the component.
-- | - `o` is the type of output messages from the component.
-- | - `eff` is the effect row for the target `Aff`
newtype DriverState h r s f g p o eff = DriverState (DriverStateRec h r s f g p o eff)

type DriverStateRec h r s f g p o eff =
  { component :: Component' h s f g p o (Aff (HalogenEffects eff))
  , state :: s
  , children :: M.Map (OrdBox p) (Ref (DriverStateX h r g eff))
  , childrenIn :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
  , childrenOut :: Ref (M.Map (OrdBox p) (Ref (DriverStateX h r g eff)))
  , mkOrdBox :: p -> OrdBox p
  , selfRef :: Ref (DriverState h r s f g p o eff)
  , handler :: o -> Aff (HalogenEffects eff) Unit
  , pendingRefs :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , pendingQueries :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , pendingOuts :: Ref (Maybe (List (Aff (HalogenEffects eff) Unit)))
  , rendering :: Maybe (r s f g p o eff)
  }

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX
  (h :: * -> * -> *)
  (r :: * -> (* -> *) -> (* -> *) -> * -> * -> # ! -> *)
  (f :: * -> *)
  (eff :: # !)

mkDriverStateXRef
  :: forall h r s f g p o eff
   . Ref (DriverState h r s f g p o eff)
  -> Ref (DriverStateX h r f eff)
mkDriverStateXRef = unsafeCoerce

unDriverStateX
  :: forall h r f eff x
   . (forall s g p o. DriverStateRec h r s f g p o eff -> x)
  -> DriverStateX h r f eff
  -> x
unDriverStateX = unsafeCoerce

-- | A wrapper of `r` from `DriverState` with the aspects relating to child
-- | components existentially hidden.
data RenderStateX
  (h :: * -> * -> *)
  (r :: * -> (* -> *) -> (* -> *) -> * -> * -> # ! -> *)
  (f :: * -> *)
  (eff :: # !)

mkRenderStateX
  :: forall h r s f g p o eff m
   . m (r s f g p o eff)
  -> m (RenderStateX h r f eff)
mkRenderStateX = unsafeCoerce

unRenderStateX
  :: forall h r f eff x
   . (forall s g p o. r s f g p o eff -> x)
  -> RenderStateX h r f eff
  -> x
unRenderStateX = unsafeCoerce

renderStateX
  :: forall m h r f eff
   . Functor m
  => (forall s g p o. Maybe (r s f g p o eff) -> m (r s f g p o eff))
  -> DriverStateX h r f eff
  -> m (RenderStateX h r f eff)
renderStateX f = unDriverStateX \st -> mkRenderStateX (f st.rendering)

renderStateX_
  :: forall m h r f eff
   . Applicative m
  => (forall s g p o. r s f g p o eff -> m Unit)
  -> DriverStateX h r f eff
  -> m Unit
renderStateX_ f = unDriverStateX \st -> traverse_ f st.rendering

initDriverState
  :: forall h r s f g p o eff
   . Component' h s f g p o (Aff (HalogenEffects eff))
  -> (o -> Aff (HalogenEffects eff) Unit)
  -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f eff))
initDriverState component handler = do
  selfRef <- newRef (unsafeCoerce {})
  childrenIn <- newRef M.empty
  childrenOut <- newRef M.empty
  pendingRefs <- newRef (Just Nil)
  pendingQueries <- newRef (component.initializer $> Nil)
  pendingOuts <- newRef (Just Nil)
  let
    ds =
      { component
      , state: component.initialState
      , children: M.empty
      , childrenIn
      , childrenOut
      , mkOrdBox: component.mkOrdBox
      , selfRef
      , handler
      , pendingRefs
      , pendingQueries
      , pendingOuts
      , rendering: Nothing
      }
  writeRef selfRef (DriverState ds)
  pure $ mkDriverStateXRef selfRef
