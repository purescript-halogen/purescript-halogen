module Halogen.Component.Profunctor (ProComponent(..)) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)
import Halogen.Component (Component, ComponentSpec, mkComponent, unComponent)
import Halogen.Query.HalogenM as HM

newtype ProComponent h f m i o = ProComponent (Component h f i o m)

derive instance newtypeProComponent :: Newtype (ProComponent h f m i o) _

instance profunctorProComponent :: Functor f => Profunctor (ProComponent h f m) where
  dimap f g (ProComponent c) =
    ProComponent (unComponent (mkComponent <<< dimapSpec f g) c)

dimapSpec
  :: forall h s f act ps i j o p m
   . Functor f
  => (j -> i)
  -> (o -> p)
  -> ComponentSpec h s f act ps i o m
  -> ComponentSpec h s f act ps j p m
dimapSpec f g spec =
  { initialState: spec.initialState <<< f
  , render: spec.render
  , eval: dimap (lmap f) (HM.mapOutput g) spec.eval
  }
