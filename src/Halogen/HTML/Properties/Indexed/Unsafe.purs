module Halogen.HTML.Properties.Indexed.Unsafe
  ( IProp(..)
  ) where

import Halogen.HTML.Core (Prop())

-- | The phantom row `r` can be thought of as a context which is synthesized in the
-- | course of constructing a refined HTML expression.
newtype IProp (r :: # *) i = IProp (Prop i)
