-- | This module provides convenience functions for creating _breadcrumb_ navigation elements.

module Halogen.Themes.Bootstrap3.Breadcrumbs 
  ( CrumbTrail(..)
  , breadcrumbs
  ) where
    
import Data.Array (map)
import Data.Tuple

import Control.Functor (($>))  

import Halogen.HTML.Target
    
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
    
import qualified Halogen.Themes.Bootstrap3 as B

-- | A `CrumbTrail` is a zipper with a current location, and crumbs behind and in front of us.
data CrumbTrail a = CrumbTrail [Tuple String (Target a)] String [Tuple String (Target a)]

-- | Create a breadcrumb navigation element from an array of `Crumb`s.
breadcrumbs :: forall m p i. (Applicative m) => CrumbTrail i -> H.HTML p (m i)
breadcrumbs (CrumbTrail behind here inFront) = 
  H.ol [ A.class_ B.breadcrumb ] 
  ( map fromCrumb behind ++ 
    [ H.li [ A.class_ B.active ] [ H.text here ] ] ++
    map fromCrumb inFront )
  where
  fromCrumb :: Tuple String (Target i) -> H.HTML p (m i)
  fromCrumb (Tuple text cr) = 
    let attr = case cr of
                 LinkTarget url -> [A.href (runURL url)]
                 DataTarget i -> [E.onclick (\_ -> E.preventDefault $> i)]
    in H.li_ [ H.a attr [ H.text text ] ]