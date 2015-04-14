-- | This module defines a type of _link targets_, which can be used as the target of a hyperlink or button.
-- |
-- | This type is quite useful when defining reusable components.

module Halogen.HTML.Target
  ( URL()
  , url
  , runURL

  , Target(..)
  , target
  ) where

import Control.Functor (($>))

import Control.Alt

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E

-- | A type-safe wrapper for a URL
data URL = URL String

-- | Create a `URL`
url :: String -> URL
url = URL

-- | Unwrap a URL
runURL :: URL -> String
runURL (URL s) = s

-- | There are two types of target:
-- |
-- | - `LinkTarget` creates a target which links to a URL.
-- | - `DataTarget` creates a target which carries data which may be used to generate inputs or requests.
data Target a = LinkTarget URL | DataTarget a

-- | Attach a `Target` to an element using the `href` or `onclick` attribute as appropriate
target :: forall i. Target i -> [A.Attr i]
target (LinkTarget url) = [ A.href (runURL url) ]
target (DataTarget i) = [ A.href "#", E.onClick (\_ -> E.preventDefault $> i) ]
