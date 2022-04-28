let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall

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
