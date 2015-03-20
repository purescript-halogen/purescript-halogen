module Data.Void where
    
newtype Void = Void Void

absurd :: forall a. Void -> a
absurd (Void v) = absurd v