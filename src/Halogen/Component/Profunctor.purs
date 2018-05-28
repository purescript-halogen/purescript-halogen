module Halogen.Component.Profunctor (ProComponent(..)) where

import Prelude
import Control.Applicative.Free (hoistFreeAp)
import Control.Monad.Free (hoistFree)
import Data.Newtype (class Newtype, over)
import Data.Profunctor (class Profunctor, lcmap)
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
  go :: forall s ps. HC.ComponentSpec h s f ps i o m -> HC.Component h f i' o' m
  go comp = HC.component $ comp
    { initialState = lcmap f comp.initialState
    , receiver = lcmap f comp.receiver
    , eval = mapOutput g <$> comp.eval
    }

mapOutput
  :: forall s f ps o o' m
   . (o -> o')
  -> HM.HalogenM s f ps o m
  ~> HM.HalogenM s f ps o' m
mapOutput f (HM.HalogenM h) = HM.HalogenM (hoistFree go h)
  where
  go :: HM.HalogenF s f ps o m ~> HM.HalogenF s f ps o' m
  go = case _ of
    HM.State s -> HM.State s
    HM.Subscribe sid es next -> HM.Subscribe sid es next
    HM.Unsubscribe sid next -> HM.Unsubscribe sid next
    HM.Lift q -> HM.Lift q
    HM.Halt msg -> HM.Halt msg
    HM.ChildQuery cq -> HM.ChildQuery cq
    HM.Raise o a -> HM.Raise (f o) a
    HM.Par p -> HM.Par (over HM.HalogenAp (hoistFreeAp (mapOutput f)) p)
    HM.Fork p -> HM.Fork (FF.hoistFork (mapOutput f) p)
    HM.GetRef p k -> HM.GetRef p k
