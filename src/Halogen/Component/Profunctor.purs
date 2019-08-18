module Halogen.Component.Profunctor (ProComponent(..)) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Data.Profunctor (class Profunctor, dimap)
import Halogen.Component (Component, ComponentSpec, mkComponent, unComponent)
import Halogen.Query.HalogenM as HM

newtype ProComponent query m input output = ProComponent (Component query input output m)

derive instance newtypeProComponent :: Newtype (ProComponent query m input output) _

instance profunctorProComponent :: Functor query => Profunctor (ProComponent query m) where
  dimap f g (ProComponent c) =
    ProComponent (unComponent (mkComponent <<< dimapSpec f g) c)

dimapSpec
  :: forall state query action slots input input' output output' m
   . Functor query
  => (input' -> input)
  -> (output -> output')
  -> ComponentSpec state query action slots input output m
  -> ComponentSpec state query action slots input' output' m
dimapSpec f g spec =
  { initialState: spec.initialState <<< f
  , render: spec.render
  , eval: dimap (lmap f) (HM.mapOutput g) spec.eval
  }
