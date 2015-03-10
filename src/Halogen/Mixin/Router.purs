module Halogen.Mixin.Router
  ( Hash()
  , runHash
  
  , onHashChange
  ) where

import DOM

import Halogen

import Control.Monad.Eff

newtype Hash = Hash String

runHash :: Hash -> String
runHash (Hash s) = s

foreign import onHashChangeImpl
  "function onHashChangeImpl(f) {\
  \  return function() {\
  \    window.addEventListener('hashchange', function() {\
  \      f();\
  \    });\
  \  };\
  \}" :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import getHash
  "function getHash() {\
  \  return window.location.hash;\
  \}" :: forall eff. Eff (dom :: DOM | eff) Hash

-- | Listen for hash change events, and provide an input to the driver function when one occurs
onHashChange :: forall i eff. (Hash -> i) -> Driver i eff -> Eff (HalogenEffects eff) Unit
onHashChange f driver = onHashChangeImpl do
  hash <- getHash
  driver (f hash)