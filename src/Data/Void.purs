module Data.Void 
  ( Void()
  , absurd
  ) where
    
-- | An empty data type, used to indicate the lack of any placeholder nodes
data Void = Void Void

absurd :: forall a. Void -> a
absurd (Void v) = absurd v