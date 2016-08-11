module Halogen.Driver.State
  ( DriverState(..)
  , DriverStateRec
  , DriverStateX
  , unDriverStateX
  , initDriverState
  )
  where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVar, putVar, makeVar)

import Data.Map as M

import DOM.HTML.Types (HTMLElement)

import Halogen.Component (Component')
import Halogen.Data.OrdBox (OrdBox)
import Halogen.Effects (HalogenEffects)
import Halogen.Internal.VirtualDOM (VTree, vtext)

import Unsafe.Coerce (unsafeCoerce)

-- | The type used to track a driver's persistent state.
newtype DriverState s f f' eff p = DriverState (DriverStateRec s f f' eff p)

type DriverStateRec s f f' eff p =
  { node :: HTMLElement
  , vtree :: VTree
  , component :: Component' s f f' (Aff (HalogenEffects eff)) p
  , state :: s
  , children :: M.Map (OrdBox p) (DriverStateX f' eff)
  , mkOrdBox :: p -> OrdBox p
  , selfRef :: AVar (DriverState s f f' eff p)
  }

-- | A version of `DriverState` with the aspects relating to child components
-- | existentially hidden.
data DriverStateX (f :: * -> *) (eff :: # !)

mkDriverStateX
  :: forall s f f' eff p
   . DriverStateRec s f f' eff p
  -> DriverStateX f eff
mkDriverStateX = unsafeCoerce

unDriverStateX
  :: forall f eff r
   . (forall s f' p. DriverStateRec s f f' eff p -> r)
  -> DriverStateX f eff
  -> r
unDriverStateX = unsafeCoerce

initDriverState
  :: forall s f f' eff p
   . HTMLElement
  -> Component' s f f' (Aff (HalogenEffects eff)) p
  -> Aff (HalogenEffects eff) (DriverStateX f eff)
initDriverState node component = do
  selfRef <- makeVar
  let
    ds =
      { node
      , vtree: vtext ""
      , component
      , state: component.initialState
      , children: M.empty
      , mkOrdBox: component.mkOrdBox
      , selfRef
      }
  putVar selfRef (DriverState ds)
  pure $ mkDriverStateX ds
