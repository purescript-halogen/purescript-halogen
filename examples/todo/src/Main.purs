module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Plus (Plus)

import Data.Const (Const())
import Data.Void (Void())

import Halogen
import Halogen.Util (appendToBody)

import Model
import Component.List
import Component.Task

ui :: forall g p. (Plus g) => InstalledComponentP State Task ListInput TaskInput g (ChildF TaskPlaceholder TaskInput) (Const Void) TaskPlaceholder p
ui = install' list mkTask

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui (installedState initialState)
  appendToBody app.node
