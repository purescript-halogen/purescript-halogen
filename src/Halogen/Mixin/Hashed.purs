-- | This module provides helper functions for working with components, where
-- | we want to save time diffing the DOM by using a hash code to detect identical
-- | component states.

module Halogen.Mixin.Hashed
  ( withHash
  ) where

import Data.Hashable

import qualified Halogen.HTML as H

-- | Lift a function which produces HTML documents to one which hashes its input.
-- |
-- | The generated HTML documents will contain the hash code of the generated output, so will not
-- | cause a re-render if the hash code does not change.
-- |
-- | **Note**: this function may be prone to false positives due to hash collisions.
withHash :: forall s a r. (Hashable s) => (s -> H.HTML a r) -> s -> H.HTML a r
withHash f s = H.hashed (hash s) (\_ -> f s)