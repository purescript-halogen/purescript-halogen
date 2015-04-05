# Module Documentation

## Module Halogen.Themes.Bootstrap3.Breadcrumbs


This module provides convenience functions for creating _breadcrumb_ navigation elements.

#### `CrumbTrail`

``` purescript
data CrumbTrail a
  = CrumbTrail [Tuple String (Target a)] String [Tuple String (Target a)]
```

A `CrumbTrail` is a zipper with a current location, and crumbs behind and in front of us.

#### `breadcrumbs`

``` purescript
breadcrumbs :: forall m p i. (Applicative m) => CrumbTrail i -> H.HTML p (m i)
```

Create a breadcrumb navigation element from an array of `Crumb`s.


## Module Halogen.Themes.Bootstrap3.InputGroup


This module provides convenience functions for creating _input groups_.

#### `AddOn`

``` purescript
data AddOn p i
  = RegularAddOn (H.HTML p i)
  | ButtonAddOn (H.HTML p i)
```

Represents an input group add-on element

We need to distinguish buttons from regular add-ons because of the 
different CSS classes

#### `inputGroup`

``` purescript
inputGroup :: forall p i. Maybe (AddOn p i) -> H.HTML p i -> Maybe (AddOn p i) -> H.HTML p i
```

Create an input group.

An input group consists of a control with optional elements placed before and after.


## Module Halogen.Themes.Bootstrap3.Navbar


This module provides convenience functions for creating Bootstrap _navbars_.

#### `NavLink`

``` purescript
type NavLink a = { target :: Target a, active :: Boolean, text :: String }
```

A simple navigation link

#### `Link`

``` purescript
type Link a = { target :: Target a, text :: String }
```

Link text and target

#### `NavDropDown`

``` purescript
type NavDropDown a = { groups :: [[Link a]], text :: String }
```


#### `NavItem`

``` purescript
data NavItem a
  = NavLink (NavLink a)
  | NavDropDown (NavDropDown a)
```

Enumerates the different kinds of navigation item

#### `Nav`

``` purescript
type Nav a = { items :: [NavItem a] }
```

A navigation menu configuration

#### `NavBarItem`

``` purescript
data NavBarItem a
  = Brand (Link a)
  | Nav (Nav a)
  | Text String
  | Button (Link a)
```

Enumerates the different kinds of item which can be rendered in a navbar

#### `NavBar`

``` purescript
type NavBar a = { items :: [NavBarItem a] }
```

A navbar configuration

#### `navbar`

``` purescript
navbar :: forall m p i. (Applicative m) => NavBar i -> H.HTML p (m i)
```

Create a navbar from a configuration object.



