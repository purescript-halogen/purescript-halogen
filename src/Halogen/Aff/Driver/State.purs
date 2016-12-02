module Halogen.Aff.Driver.State
  ( ComponentType(..)
  , DriverState(..)
  , DriverStateRec
  , DriverStateX
  , unDriverStateX
  , initDriverState
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (Ref, newRef, writeRef)

import Data.List (List(..))
import Data.Map as M
import Data.Maybe (Maybe(..))

import Halogen.Component (Component')
import Halogen.Data.OrdBox (OrdBox)
import Halogen.Effects (HalogenEffects)

import Unsafe.Coerce (unsafeCoerce)

-- | A type used to track which type of component a driver state value is for.
data ComponentType = Root | Child

derive instance eqComponentType :: Eq ComponentType
derive instance ordComponentType :: Ord ComponentType

instance showComponentType :: Show ComponentType where
  show Root = "Root"
  show Child = "Child"

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
  , componentType :: ComponentType
  , state :: s
  , children :: M.Map (OrdBox p) (Ref (DriverStateX h r g eff))
  , mkOrdBox :: p -> OrdBox p
  , selfRef :: Ref (DriverState h r s f g p o eff)
  , handler :: o -> Aff (HalogenEffects eff) Unit
  , pendingIn :: Maybe (List (Aff (HalogenEffects eff) Unit))
  , pendingOut :: Maybe (List o)
  , keyId :: Int
  , fresh :: Ref Int
  , rendering :: Maybe r
  }

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX (h :: * -> * -> *) r (g :: * -> *) (eff :: # !)

mkDriverStateXVar
  :: forall h r s f g p o eff
   . Ref (DriverState h r s f g p o eff)
  -> Ref (DriverStateX h r f eff)
mkDriverStateXVar = unsafeCoerce

unDriverStateX
  :: forall h r f eff x
   . (forall s g p o. DriverStateRec h r s f g p o eff -> x)
  -> DriverStateX h r f eff
  -> x
unDriverStateX = unsafeCoerce

initDriverState
  :: forall h r s f g p o eff
   . Component' h s f g p o (Aff (HalogenEffects eff))
  -> ComponentType
  -> (o -> Aff (HalogenEffects eff) Unit)
  -> Int
  -> Ref Int
  -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f eff))
initDriverState component componentType handler keyId fresh = do
  selfRef <- newRef (unsafeCoerce {})
  let
    ds =
      { component
      , componentType
      , state: component.initialState
      , children: M.empty
      , mkOrdBox: component.mkOrdBox
      , selfRef
      , keyId
      , fresh
      , handler
      , pendingIn: component.initializer $> Nil
      , pendingOut: component.initializer $> Nil
      , rendering: Nothing
      }
  writeRef selfRef (DriverState ds)
  pure $ mkDriverStateXVar selfRef
