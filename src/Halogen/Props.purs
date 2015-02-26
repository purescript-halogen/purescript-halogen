module Halogen.Props where

import Data.Monoid

import Control.Monad.Eff

data Props

-- | Very unsafe, need to modify vdom bindings?
foreign import propsToRecord
  "function propsToRecord(props) {\
  \  return props;\
  \}" :: forall r. Props -> Object r

-- | Slow, use ST
foreign import stringProp
  "function stringProp(key) {\
  \  return function(value) {\
  \    var props = {};\
  \    props[key] = value;\
  \    return props;\
  \  };\
  \}" :: String -> String -> Props

foreign import handlerProp
  "function handlerProp(key) {\
  \  return function(f) {\
  \    var props = {};\
  \    props[key] = function(e) {\
  \      f(e)();\
  \    };\
  \    return props;\
  \  };\
  \}" :: forall eff event. String -> (event -> Eff eff Unit) -> Props

foreign import combineProps
  "function combineProps(p1) {\
  \  return function(p2) {\
  \    var props = {};\
  \    for (var k in p1) {\
  \      props[k] = p1[k];\
  \    }\
  \    for (var k in p2) {\
  \      props[k] = p2[k];\
  \    }\
  \    return props;\
  \  };\
  \}" :: Props -> Props -> Props
  
instance semigroupProps :: Semigroup Props where
  (<>) = combineProps

foreign import emptyProps "var emptyProps = {}" :: Props

instance monoidProps :: Monoid Props where
  mempty = emptyProps