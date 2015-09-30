module Halogen.HTML.Properties.Indexed.Unsafe
  ( IProp(..)
  ) where

import Halogen.HTML.Core (Prop())

-- | The phantom row `ρ` can be thought of as a context which is synthesized in the
-- | course of constructing a refined HTML expression.
newtype IProp (ρ :: # *) i = IProp (Prop i)
