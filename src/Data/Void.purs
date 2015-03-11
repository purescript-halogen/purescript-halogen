module Data.Void 
  ( Void()
  , absurd
  ) where
    
-- | An empty data type
data Void = Void Void

-- | Since the `Void` type has no inhabitants, we can eliminate it producing any type whatsoever.
absurd :: forall a. Void -> a
absurd (Void v) = absurd v