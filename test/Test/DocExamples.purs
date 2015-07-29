module Test.DocExamples where

import Prelude
import Halogen
import Control.Monad.Aff
import Control.Monad.Free

data Input1 a = Tick a

instance functorInput1 :: Functor Input1 where
  map f (Tick a) = Tick (f a)

sendTick :: forall eff. Driver (Free Input1) eff -> Aff (HalogenEffects eff) Unit
sendTick driver = driver (actionF Tick)

data Input2 a = GetTickCount (Int -> a)

instance functorInput2 :: Functor Input2 where
  map f (GetTickCount k) = GetTickCount (f <<< k)

getTickCount :: forall eff. Driver (Free Input2) eff -> Aff (HalogenEffects eff) Int
getTickCount driver = driver (requestF GetTickCount)
