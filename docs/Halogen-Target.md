# Module Documentation

## Module Halogen.HTML.Target


This module defines a type of _link targets_, which can be used as the target of a hyperlink or button.

This type is quite useful when defining reusable components.

#### `URL`

``` purescript
data URL
```

A type-safe wrapper for a URL

#### `url`

``` purescript
url :: String -> URL
```

Create a `URL`

#### `runURL`

``` purescript
runURL :: URL -> String
```

Unwrap a URL

#### `Target`

``` purescript
data Target a
  = LinkTarget URL
  | DataTarget a
```

There are two types of target:

- `LinkTarget` creates a target which links to a URL.
- `DataTarget` creates a target which carries data which may be used to generate inputs or requests.

#### `target`

``` purescript
target :: forall m i. (Applicative m) => Target i -> [A.Attr (m i)]
```

Attach a `Target` to an element using the `href` or `onclick` attribute as appropriate



