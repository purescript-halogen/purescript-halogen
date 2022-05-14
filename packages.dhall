let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220507/packages.dhall
        sha256:cf54330f3bc1b25a093b69bff8489180c954b43668c81288901a2ec29a08cc64

in  upstream
  with dom-indexed =
    { repo = "https://github.com/purescript-halogen/purescript-dom-indexed.git"
    , version = "v10.1.0"
    , dependencies = upstream.dom-indexed.dependencies
    }
