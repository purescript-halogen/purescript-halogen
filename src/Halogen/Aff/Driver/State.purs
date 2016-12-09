module Halogen.Aff.Driver.State
  ( DriverState(..)
  , DriverStateRec
  , DriverStateX
  , unDriverStateX
  , mkDriverStateXRef
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
  , mkOrdBox :: p -> OrdBox p
  , selfRef :: Ref (DriverState h r s f g p o eff)
  , handler :: o -> Aff (HalogenEffects eff) Unit
  , pendingIn :: Maybe (List (Aff (HalogenEffects eff) Unit))
  , pendingOut :: Maybe (List o)
  , keyId :: Int
  , fresh :: Ref Int
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

initDriverState
  :: forall h r s f g p o eff
   . Component' h s f g p o (Aff (HalogenEffects eff))
  -> (o -> Aff (HalogenEffects eff) Unit)
  -> Int
  -> Ref Int
  -> Eff (HalogenEffects eff) (Ref (DriverStateX h r f eff))
initDriverState component handler keyId fresh = do
  selfRef <- newRef (unsafeCoerce {})
  let
    ds =
      { component
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
  pure $ mkDriverStateXRef selfRef
