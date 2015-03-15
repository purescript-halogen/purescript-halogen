-- | This module provides convenience functions for creating _breadcrumb_ navigation elements.

module Halogen.Themes.Bootstrap3.Breadcrumbs 
  ( Crumb(..)
  , CrumbTrail(..)
  , breadcrumbs
  
  , URL()
  , url
  , runURL
  ) where
    
import Data.Array (map)
import Data.Tuple

import Control.Functor (($>))  
    
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
    
import qualified Halogen.Themes.Bootstrap3 as B

-- | A type-safe wrapper for a URL
data URL = URL String

-- | Create a `URL`
url :: String -> URL
url = URL

-- | Unwrap a URL
runURL :: URL -> String
runURL (URL s) = s

-- | There are two types of crumbs:
-- | 
-- | - `LinkCrumb` creates crumbs which link to URLs.
-- | - `DataCrumb` contains data which may be used to generate inputs or requests.
-- |
data Crumb a = LinkCrumb URL | DataCrumb a

-- | A `CrumbTrail` is a zipper with a current location, and crumbs behind and in front of us.
data CrumbTrail a = CrumbTrail [Tuple String (Crumb a)] String [Tuple String (Crumb a)]

-- | Create a breadcrumb navigation element from an array of `Crumb`s.
breadcrumbs :: forall a i. CrumbTrail i -> H.HTML a i
breadcrumbs (CrumbTrail behind here inFront) = 
  H.ol (A.class_ B.breadcrumb) 
  ( map fromCrumb behind ++ 
    [ H.li (A.class_ B.active) [ H.text here ] ] ++
    map fromCrumb inFront )
  where
  fromCrumb :: Tuple String (Crumb i) -> H.HTML a i
  fromCrumb (Tuple text cr) = 
    let attr = case cr of
                 LinkCrumb url -> A.href (runURL url)
                 DataCrumb i -> E.onclick (\_ -> E.preventDefault $> i)
    in H.li_ [ H.a attr [ H.text text ] ]