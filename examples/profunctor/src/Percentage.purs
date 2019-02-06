module Example.Profunctor.Percentage (Percentage(..), Query, Slot, percentageInput, percentageInputPro) where

import Prelude
import Data.Int as Int
import Data.Newtype (class Newtype, un)
import Data.Profunctor (dimap)
import Example.Profunctor.Ratio (Query) as RatioInput
import Example.Profunctor.Ratio (Ratio(..), ratioInputPro)
import Halogen as H
import Halogen.Component.Profunctor (ProComponent(..))
import Halogen.HTML as HH

type Slot = H.Slot Query Percentage

newtype Percentage = Percentage Int
derive instance newtypePercentage :: Newtype Percentage _

instance showPercentage :: Show Percentage where
  show (Percentage p) = show p <> "%"

percentageFromRatio :: Ratio -> Percentage
percentageFromRatio = Percentage <<< Int.floor <<< (*) 100.0 <<< un Ratio

percentageToRatio :: Percentage -> Ratio
percentageToRatio = Ratio <<< flip (/) 100.0 <<< Int.toNumber <<< un Percentage

type Query = RatioInput.Query

percentageInputPro :: forall m. ProComponent HH.HTML Query m Percentage Percentage
percentageInputPro = dimap percentageToRatio percentageFromRatio ratioInputPro

percentageInput :: forall m. H.Component HH.HTML Query Percentage Percentage m
percentageInput = un ProComponent percentageInputPro
