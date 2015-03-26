-- | This module provides convenience functions for creating Bootstrap _navbars_.

module Halogen.Themes.Bootstrap3.Navbar 
  ( NavBarItem(..)
  , NavItem(..)
  , NavBar()
  , NavLink()
  , Nav()
  , NavDropDown()
  , Link()

  , navbar
  ) where
    
import Data.Array (map)
import Data.Monoid (mempty)
import Data.Foldable (intercalate)

import Halogen.HTML.Target

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Handler as E
    
import qualified Halogen.Themes.Bootstrap3 as B

-- | A simple navigation link
type NavLink a =
  { text :: String
  , active :: Boolean
  , target :: Target a
  }

-- | Link text and target
type Link a = 
  { text :: String
  , target :: Target a
  }
  
type NavDropDown a =
  { text :: String
  , groups :: [[Link a]]
  }

-- | Enumerates the different kinds of navigation item
data NavItem a
  = NavLink (NavLink a)
  | NavDropDown (NavDropDown a)

-- | A navigation menu configuration
type Nav a =
  { items :: [NavItem a]
  }

-- | Enumerates the different kinds of item which can be rendered in a navbar
data NavBarItem a
  = Brand (Link a)
  | Nav (Nav a)
  | Text String
  | Button (Link a)

-- | A navbar configuration
type NavBar a =
  { items :: [NavBarItem a]
  }

-- | Create a navbar from a configuration object.
navbar :: forall a i node. (H.HTMLRepr node) => NavBar i -> node a i
navbar conf = 
  H.nav [ A.classes [B.navbar, B.navbarDefault] ]
        [ H.div [ A.class_ B.containerFluid ] (map renderItem conf.items) ]
        
  where
  renderItem :: NavBarItem i -> node a i
  renderItem (Brand o) = H.a (A.class_ B.navbarBrand : target o.target) [H.text o.text]
  renderItem (Nav o) = H.ul [ A.classes [B.nav, B.navbarNav] ] (map renderNavItem o.items)
  renderItem (Text s) = H.p [ A.class_ B.navbarText ] [H.text s]
  renderItem (Button o) = H.a (A.classes [B.btn, B.btnDefault, B.navbarBtn] : target o.target) [H.text o.text]
  
  renderNavItem :: NavItem i -> node a i
  renderNavItem (NavLink o) = H.li (if o.active then [A.class_ B.active] else []) [ H.a (target o.target) [ H.text o.text ] ]
  renderNavItem (NavDropDown o) = H.li [ A.class_ B.dropdown ] 
                                       [ H.a [ A.href "#", A.class_ B.dropdownToggle ] [H.text o.text] 
                                       , H.ul [ A.class_ B.dropdownMenu ] (intercalate [divider] (map (map renderLink) o.groups))
                                       ]
    where
    divider :: node a i
    divider = H.li [ A.class_ B.divider ] []
        
    renderLink :: Link i -> node a i
    renderLink o = H.li_ [ H.a (target o.target) [ H.text o.text ] ]
