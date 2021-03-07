let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.0-20210304/packages.dhall sha256:c88151fe7c05f05290224c9c1ae4a22905060424fb01071b691d3fe2e5bad4ca

in  upstream
  with halogen-subscriptions =
    { version = "main"
    , repo = "https://github.com/purescript-halogen/purescript-halogen-subscriptions"
    , dependencies =
        [ "arrays"
        , "effect"
        , "foldable-traversable"
        , "functors"
        , "refs"
        , "safe-coerce"
        , "unsafe-reference"
        ]
    }
