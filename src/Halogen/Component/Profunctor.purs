module Halogen.Component.Profunctor (ProComponent(..)) where

import Prelude
import Control.Applicative.Free (hoistFreeAp)
import Control.Monad.Free (hoistFree)
import Data.Newtype (class Newtype, over)
import Data.Profunctor (class Profunctor, lmap)
import Halogen.Component as HC
import Halogen.Query.HalogenM as HM
import Halogen.Query.ForkF as FF

newtype ProComponent h f m i o = ProComponent (HC.Component h f i o m)

derive instance newtypeProComponent :: Newtype (ProComponent h f m i o) _

instance profunctorProComponent :: Profunctor (ProComponent h f m) where
  dimap = dimapProComponent

dimapProComponent
  :: forall h f m i i' o o'
   . (i' -> i)
  -> (o -> o')
  -> ProComponent h f m i o
  -> ProComponent h f m i' o'
dimapProComponent f g (ProComponent c) = ProComponent (HC.unComponent go c)
  where
  go :: forall s g p. HC.Component' h s f g p i o m -> HC.Component h f i' o' m
  go comp = HC.mkComponent $ comp
    { initialState = lmap f comp.initialState
    , receiver = lmap f comp.receiver
    , eval = mapOutput g <$> comp.eval
    }

mapOutput
  :: forall s f g p o o' m
   . (o -> o')
  -> HM.HalogenM s f g p o m
  ~> HM.HalogenM s f g p o' m
mapOutput f (HM.HalogenM h) = HM.HalogenM (hoistFree go h)
  where
  go :: HM.HalogenF s f g p o m ~> HM.HalogenF s f g p o' m
  go = case _ of
    HM.State s -> HM.State s
    HM.Subscribe es next -> HM.Subscribe es next
    HM.Lift q -> HM.Lift q
    HM.Halt msg -> HM.Halt msg
    HM.GetSlots k -> HM.GetSlots k
    HM.CheckSlot p k -> HM.CheckSlot p k
    HM.ChildQuery p cq -> HM.ChildQuery p cq
    HM.Raise o a -> HM.Raise (f o) a
    HM.Par p -> HM.Par (over HM.HalogenAp (hoistFreeAp (mapOutput f)) p)
    HM.Fork p -> HM.Fork (FF.hoistFork (mapOutput f) p)
    HM.GetRef p k -> HM.GetRef p k
