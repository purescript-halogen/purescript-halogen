let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall
        sha256:66cdee258ce77574cb12898d824ea7ed89336ba6ea5087146c18dcbaa28327b7

in  upstream
  with halogen-subscriptions =
    { version = "purs-0.15"
    , repo =
        "https://github.com/purescript-halogen/purescript-halogen-subscriptions.git"
    , dependencies =
      [ "arrays"
      , "contravariant"
      , "control"
      , "effect"
      , "foldable-traversable"
      , "maybe"
      , "prelude"
      , "refs"
      , "safe-coerce"
      , "unsafe-reference"
      ]
    }
  with halogen-vdom =
    { version = "purs-0.15"
    , repo = "https://github.com/purescript-halogen/purescript-halogen-vdom.git"
    , dependencies =
      [ "bifunctors"
      , "effect"
      , "foreign"
      , "foreign-object"
      , "maybe"
      , "prelude"
      , "refs"
      , "tuples"
      , "unsafe-coerce"
      , "web-html"
      ]
    }
