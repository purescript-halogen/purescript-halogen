module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Halogen
import Halogen.Util (appendToBody)

import Model
import Component.List
import Component.Task

ui :: forall g. (Plus g) => InstalledComponent State Task ListInput TaskInput g ListSlot
ui = install' list mkTask
  where
  mkTask (ListSlot _) = createChild task { description: "", completed: false }

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
