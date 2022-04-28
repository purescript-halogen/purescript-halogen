let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/prepare-0.15/src/packages.dhall
        sha256:30d10fa9332cfe1b7300d51d1d35337c3b67ab4ec42a0ac9b9f8dce48851631c

in  upstream
  with
    halogen-subscriptions =
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

  with
    halogen-vdom =
      { version = "purs-0.15"
      , repo =
          "https://github.com/purescript-halogen/purescript-halogen-vdom.git"
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
