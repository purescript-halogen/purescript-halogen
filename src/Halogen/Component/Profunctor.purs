module Halogen.Component.Profunctor (ProComponent(..)) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)
import Halogen.Component (Component, ComponentSpec, mkComponent, unComponent)
import Halogen.Query.HalogenM as HM

newtype ProComponent surface query m input output = ProComponent (Component surface query input output m)

derive instance newtypeProComponent :: Newtype (ProComponent surface query m input output) _

instance profunctorProComponent :: Functor query => Profunctor (ProComponent surface query m) where
  dimap f g (ProComponent c) =
    ProComponent (unComponent (mkComponent <<< dimapSpec f g) c)

dimapSpec
  :: forall surface state query action slots input input' output output' m
   . Functor query
  => (input' -> input)
  -> (output -> output')
  -> ComponentSpec surface state query action slots input output m
  -> ComponentSpec surface state query action slots input' output' m
dimapSpec f g spec =
  { initialState: spec.initialState <<< f
  , render: spec.render
  , eval: dimap (lmap f) (HM.mapOutput g) spec.eval
  }
