-- | This module implements an extremely slow fibonacci function, which can be
-- | used to observe the improvement provided by the `memoized` and `lazy`
-- | functions in render code.
module Example.MemoizedLazy.Fibonacci where

import Prelude

import Data.Array (fold)
import Halogen.HTML as HH

-- | Render the number at the given position in the Fibonacci sequence.
renderFib :: forall w i. Int -> HH.HTML w i
renderFib n =
  HH.text $ fold
    [ "The Fibonacci number for "
    , show n
    , " is "
    , show $ fib n
    , "."
    ]

-- | An extremely slow implementation of the Fibonacci function.
fib :: Int -> Int
fib 1 = 1
fib 0 = 0
fib n = fib (n - 1) + fib (n - 2)
