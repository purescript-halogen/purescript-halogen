module Test.Main where

import Prelude
import Effect (Effect)
import Control.Monad.Eff.Console (CONSOLE, log)

-- This isn't a real test suite, we're just checking that the examples used in
-- the documentation compile.

main :: Eff (console :: CONSOLE) Unit
main = log "Nothing to see here"
