module Debug.Trace where

foreign import trace :: forall a b. a -> b -> b
